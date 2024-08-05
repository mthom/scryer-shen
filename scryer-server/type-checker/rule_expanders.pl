:- module(rule_expanders, []).

:- use_module(library(dcgs)).
:- use_module(library(lists)).

:- use_module(type_variables).

list_comma_goals([], EndTerm, EndTerm).
list_comma_goals([G|Gs], EndTerm, (G, EGs)) :-
    list_comma_goals(Gs, EndTerm, EGs).

type_check_goals(type_check(X, T)) -->
    [[g(type_check(X, T))]].

provability_goals(type_check(X, T)) -->
    [[h(type_check(X, T))]].
provability_goals(G) -->
    [[g(G)]].

:- meta_predicate expand_shen_goals(?, 3, ?, ?).

expand_shen_goals((G1, G2), GoalRenderer) -->
    expand_shen_goals(G1, GoalRenderer),
    expand_shen_goals(G2, GoalRenderer).
expand_shen_goals(type_check(X, A), GoalRenderer) -->
    phrase(GoalRenderer, type_check(X, A)).
expand_shen_goals(>>(Gs, P), _GoalRenderer) -->
    foldl(provability_goals, Gs),
    premises_proof_end_terms([], P).
expand_shen_goals(g(G), _GoalRenderer) -->
    [[g(G)]].

premises_proof_end_terms([P|Ps], Conclusion) -->
    [g(provable([P|Ps]))],
    premises_proof_end_terms([], Conclusion).
premises_proof_end_terms([], Conclusion) -->
    (  { var(Conclusion) } ->
       []
    ;  [[h(provable(Conclusion))]]
    ).

user:term_expansion((>>([type_check(X, A)|Ps], P) :- Goals), (inference_rules:provable(type_check(X, A)) --> ExpandedGoals)) :-
    type(A),
    phrase(expand_shen_goals(Goals, provability_goals), ListOfExpandedGoals),
    phrase(premises_proof_end_terms(Ps, P), EndTerms),
    list_comma_goals(ListOfExpandedGoals, EndTerms, ExpandedGoals).
user:term_expansion(>>([type_check(X, A)|Ps], P), (inference_rules:provable(type_check(X, A)) --> ExpandedGoals)) :-
    type(A),
    phrase(premises_proof_end_terms(Ps, P), ListOfExpandedGoals),
    list_comma_goals(ListOfExpandedGoals, [], ExpandedGoals).
user:term_expansion((type_check(X, A) :- Goals), (inference_rules:type_check(X, A) --> ExpandedGoals)) :-
    type(A),
    phrase(expand_shen_goals(Goals, type_check_goals), ListOfExpandedGoals),
    list_comma_goals(ListOfExpandedGoals, [], ExpandedGoals).
user:term_expansion(type_check(X, T), (inference_rules:type_check(X, T) --> [])).
