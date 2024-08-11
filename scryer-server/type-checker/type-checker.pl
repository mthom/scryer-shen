:- module(type_checker, [op(599, fx, ?),
                         set_maximum_allowed_inferences/2,
                         start_proof/3]).

:- use_module(library(dcgs)).
:- use_module(library(iso_ext), [call_with_inference_limit/3]).
:- use_module(library(lambda)).
:- use_module(library(lists)).

:- use_module(proof_trees).
:- use_module(inference_rules).
:- use_module(rule_expanders).
:- use_module(term_variables).
:- use_module(type_variables).

:- set_prolog_flag(occurs_check, true).

:- dynamic(inf_limit_exceeded/0).

:- dynamic(maximum_allowed_inferences/1).

:- meta_predicate depth_iterated_proof(2, ?, ?, ?).

assert_inf_limit_exceeded :-
    (  inf_limit_exceeded ->
       true
    ;  assertz(inf_limit_exceeded)
    ).

set_maximum_allowed_inferences(OldInf, NewInf) :-
    integer(NewInf),
    NewInf > 0,
    retract(maximum_allowed_inferences(OldInf)),
    assertz(maximum_allowed_inferences(NewInf)).

maximum_allowed_inferences(65536). % default maximum inferences is 2^16

attribute_hyp(type_check(X, T)) :-
    term(X),
    type(T).

start_proof(Hyps, type_check(X, T), ProofTree) :-
    retractall(inf_limit_exceeded),
    maplist(attribute_hyp, Hyps),
    term(X),
    Goal = (Hyps, X, T)+\Infs^ProofTree^prove(Infs, [], Hyps, g(type_check(X, T)), ProofTree),
    call_with_inference_limit(
        depth_iterated_proof(Goal, ProofTree, Status, 32),
        50_000,
        R
    ),
    (  R == inference_limit_exceeded ->
       throw(type_check_error(inference_limit_exceeded))
    ;  Status = false,
       !,
       false
    ;  Status = true ->
       true
    ;  throw(type_check_error(Status))
    ).

depth_iterated_proof(Goal, ProofTree, Status, Inferences) :-
    length(InfList, Inferences),
    (  call(Goal, InfList, ProofTree),
       verify_proof_tree(ProofTree),
       Status = true
    ;  retract(inf_limit_exceeded),
       NextInferences is 2 * Inferences,
       (  maximum_allowed_inferences(MaxInferences),
          NextInferences >= MaxInferences,
          Status = depth_inference_limit_exceeded
       ;  depth_iterated_proof(Goal, ProofTree, Status, NextInferences)
       )
    ;  Status = false % if the proof failed within the inference
                      % limit, we cannot prove the Goal so it's no
                      % help increasing the inference limit: fail
                      % immediately
    ).


shen_if_condition(G) :-
    cont:shift(bind(G, F)),
    F \== false.

precedent_check(_AncestorList, Hyps, Goal, assumed(g(Goal))) :-
    member(Goal, Hyps).
precedent_check(AncestorList, _Hyps, Goal, fix_point(g(Goal))) :-
    member(g(Goal), AncestorList).

prove_type_check(g(type_check(X, T)), Hyps, Hyps, X, T) -->
    { term(X) },
    inference_rules:type_check(X, T).
prove_type_check(discharged(Hyp), PrevHyps, SuccHyps, X, T) -->
    { select(Hyp, PrevHyps, SuccHyps) },
    inference_rules:provable(Hyp),
    [g(type_check(X, T))].

affirm_hypothesis(type_check(X, T)) :-
    attribute_hyp(type_check(X, T)),
    value_type(X, T).
affirm_hypothesis(provable([G|Gs])) :-
    maplist(affirm_hypothesis, [G|Gs]).
affirm_hypothesis(provable(G)) :-
    callable(G),
    G \= [_|_],
    affirm_hypothesis(G).
affirm_hypothesis(type_eq(T, U)) :-
    type_eq(T, U).
affirm_hypothesis(exists(T)) :-
    exists(T).

succ_hyps(Hyps, h(provable([H|Hs])), SuccHyps) :-
    append([H|Hs], Hyps, SuccHyps).
succ_hyps(Hyps, h(provable(H)), [H|Hyps]).
succ_hyps(Hyps, h(Goal), [Goal|Hyps]).
succ_hyps(Hyps, g(_),    Hyps).

succ_ancestor_list(g(type_check(X, T)), AncestorList, [g(type_check(X,T)) | AncestorList]).
succ_ancestor_list(Discharged, AncestorList, AncestorList) :-
    Discharged \= g(type_check(_X, _T)).

chain_proof([], _SuccIs, _AL, _SuccAL, _Hs, _SuccHs, _SubTs, _SuccSubTs, _Cs) -->
    { assert_inf_limit_exceeded,
      false }.
chain_proof([t|Is], [t|Is], AL, AL, Hs, Hs, SubTs, SubTs, []) -->
    [].
chain_proof([t|Is], SuccIs, AL, SuccAL, Hs, SuccHs, SubTs, SuccSubTs, [C|Cs]) -->
    (  { prove([t|Is], AL, Hs, C, SubT),
         succ_hyps(Hs, C, NewHs),
         succ_ancestor_list(C, AL, NewAL),
         SubTs = [SubT | RSubTs] },
       chain_proof(Is, SuccIs, NewAL, SuccAL, NewHs, SuccHs, RSubTs, SuccSubTs, Cs)
    ;  { C = h(_) },
       [C],
       chain_proof(Is, SuccIs, AL, SuccAL, Hs, SuccHs, SubTs, SuccSubTs, Cs)
    ).

chain_proof(_Is, _AL, _Hs, [], []).
chain_proof(Is, AL, Hs, [C|Cs], SubTs) :-
    length([C|Cs], N),
    phrase(chain_proof(Is, SuccIs, AL, SuccAL, Hs, SuccHs, SubTs, SuccSubTs, [C|Cs]), FailureQueue),
    (  length(FailureQueue, N) ->
       false
    ;  chain_proof(SuccIs, SuccAL, SuccHs, FailureQueue, SuccSubTs)
    ).

chained_proof_tree(discharged(Hyp), Goal, Subtrees, t(discharged(Hyp), Subtree)) :-
    Subtree =.. [t, Goal | Subtrees].
chained_proof_tree(_TreeHead, Goal, Subtrees, Tree) :-
    Tree =.. [t, Goal | Subtrees].

prove([], _AncestorList, _Hyps, _Goal, _Tree) :-
    assert_inf_limit_exceeded,
    false.
prove([t|Infs], AncestorList, Hyps, g(type_check(X, T)), Tree) :-
    (  precedent_check(AncestorList, Hyps, type_check(X, T), Tree)
    ;  phrase(prove_type_check(TreeHead, Hyps, SuccHyps, X, T), Conditions),
       succ_ancestor_list(TreeHead, AncestorList, SuccAncestorList),
       chain_proof(Infs, SuccAncestorList, SuccHyps, Conditions, Subtrees),
       chained_proof_tree(TreeHead, g(type_check(X, T)), Subtrees, Tree)
    ).
prove([t|Infs], AncestorList, Hyps, g(provable([Goal|Goals])), Tree) :-
    maplist((Infs, AncestorList, Hyps)+\G^ST^prove(Infs, AncestorList, Hyps, g(provable(G)), ST),
            [Goal|Goals],
            Subtrees),
    Tree =.. [t, g(provable([Goal|Goals])) | Subtrees].
prove([t|Infs], AncestorList, Hyps, g(provable(Goal)), Tree) :-
    callable(Goal),
    (  precedent_check(AncestorList, Hyps, provable(Goal), Tree)
    ;  affirm_hypothesis(Goal),
       phrase(provable(Goal), Conditions),
       chain_proof(Infs, [g(provable(Goal))|AncestorList], [Goal|Hyps], Conditions, Subtrees),
       chained_proof_tree(g(provable(Goal)), g(provable(Goal)), Subtrees, Tree)
    ).
prove([t|_Infs], _AncestorList, _Hyps, g(Goal), t(g(Goal))) :-
    callable(Goal),
    \+ functor(Goal, type_check, 2),
    \+ functor(Goal, provable, 1),
    call(Goal).
prove([t|_Infs], _AncestorList, _Hyps, h(Goal), assumed(g(Goal))) :-
    affirm_hypothesis(Goal).
