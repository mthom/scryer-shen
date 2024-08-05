:- module('$scryer-shen-ipc', [continue/2, write_error/1]).

:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(cont)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lambda)).
:- use_module(library(lists)).

:- meta_predicate continue(0, ?).

continue(Query, VNs) :-
    reset(Query, Ball, Cont),
    (  var(Ball) ->
       write([true, false]),
       nl
    ;  pause_or_return(Cont, Ball, VNs)
    ),
    !.
continue(_, _) :-
    write([false, false]),
    nl.

write_canonical_term_wq(Term, VNs) :-
    write_term(Term, [ignore_ops(true), variable_names(VNs),
                      quoted(true), double_quotes(true)]).

write_error(Error) :-
    write_canonical_term_wq(Error, []),
    nl.

eq_list_match(L, [L=V1|_], V1).
eq_list_match(L, [_|VNs], V) :-
    eq_list_match(L, VNs, V).

label_matches(_, [], [], _).
label_matches(VNs0, [L=V1|VNs1], Matches, N) :-
    !,
    N1 is N+1,
    (  eq_list_match(L, VNs0, V0),
       V0 \== V1 ->
       Matches = [N|Matches0],
       label_matches(VNs0, VNs1, Matches0, N1)
    ;  label_matches(VNs0, VNs1, Matches, N1)
    ).

provisional_variable_labels([], _, _, []).
provisional_variable_labels([V|Vs], NumVars0, Min, VNs) :-
    NumVars is NumVars0 + Min - 1,
    numlist(Min, NumVars, VarLabels),
    maplist(\V^N^VarForm^(
                write_term_to_chars('$VAR'(N), [numbervars(true)], VCs),
                atom_chars(VA, VCs),
                VarForm = (VA = V)
            ),
            [V|Vs],
            VarLabels,
            VNs
           ).

matching_variable_labels_loop(TermVars, NumVars0, Min, VNs0, VNs) :-
    provisional_variable_labels(TermVars, NumVars0, Min, VNs1),
    label_matches(VNs0, VNs1, Matches, Min),
    (   Matches == [] ->
        append(VNs0, VNs1, VNs)
    ;   list_max(Matches, Max),
        % Max1 is Max + 1,
        matching_variable_labels_loop(TermVars, NumVars0, Max, VNs0, VNs)
    ).

variable_labels(Term, VNs0, VNs) :-
    term_variables(Term, TermVars),
    length(TermVars, NumVars0),
    matching_variable_labels_loop(TermVars, NumVars0, 0, VNs0, VNs).

pause_or_return(none, Ball, VNs) :-
    function_eval(Ball, VNs).
pause_or_return(cont(Cont), bind(F,X), VNs) :-
    !,
    function_eval(bind(F,X), VNs),
    continue(Cont, VNs).
pause_or_return(cont(_Cont), return_to_shen(T), VNs0) :-
    variable_labels(T, VNs0, VNs),
    function_eval(return_to_shen(T), VNs).
pause_or_return(cont(_Cont), type_check_return_to_shen(T), VNs0) :-
    variable_labels(T, VNs0, VNs),
    function_eval(type_check_return_to_shen(T), VNs).

list_dot_functor([], []).
list_dot_functor([T|Ts], '.'(T, Us)) :-
    list_dot_functor(Ts, Us).

shen_functor(T, T) :-
    (  atomic(T)
    ;  var(T)
    ),
    !.
shen_functor([T|Ts], '.'('.', U, Us)) :-
    shen_functor(T, U),
    shen_functor(Ts, Us),
    !.
shen_functor(T, '.'(F, Vs)) :-
    T =.. [F | Ts],
    maplist(shen_functor, Ts, Us),
    list_dot_functor(Us, Vs).

function_eval(bind(F,X), VNs) :-
    shen_functor(F, SF),
    write_canonical_term_wq([SF, true], VNs), % write to scryer-shen
                                              % which is listening to
                                              % stdout ...
    nl,
    read(X).                          % .. and block until the result
                                      % is read back from scryer-shen.
function_eval(return_to_shen(T), VNs) :-
    shen_functor(T, TF),
    write_canonical_term_wq([TF, false], VNs),
    nl.
function_eval(type_check_return_to_shen(T), VNs) :-
    shen_functor(T, TF),
    write_canonical_term_wq([[type_functor, TF]], VNs),
    nl.
