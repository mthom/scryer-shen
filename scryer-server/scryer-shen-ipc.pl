:- module(ipc, [bind/2,
                continue/2,
                if_bind/2,
                return_to_shen/1,
                type_check_return_to_shen/1,
                write_error/1]).

:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(iso_ext)).
:- use_module(library(lambda)).
:- use_module(library(lists)).

:- meta_predicate continue(0, ?).

continue(Query, VNs) :-
    bb_put('#%variable_names', VNs),
    (  catch(Query, '#%return_success'(P), return_handler(P)) ->
       (  var(P) ->
          write([true, false]),
          nl
       ;  true
       )
    ;  write([false, false]),
       nl
    ).

return_handler(Printer) :-
    callable(Printer),
    call(Printer).

bind(F, X) :-
    bb_get('#%variable_names', VNs),
    executable_functor_shen_expr(F, SF),
    write_canonical_term_wq([SF, true], VNs), % write to scryer-shen
                                              % which is listening to
                                              % stdout ...
    nl,
    read(X). % .. and block until the result is read back from
             % scryer-shen.

if_bind(F, X) :-
    bb_get('#%variable_names', VNs),
    executable_functor_shen_expr(F, SF),
    write_canonical_term_wq([if_bind, SF, true], VNs),
    nl,
    read(X).

return_to_shen(T) :-
    bb_get('#%variable_names', VNs0),
    variable_labels(T, VNs0, VNs),
    data_functor_shen_expr(T, TF),
    throw('#%return_success'((ipc:write_canonical_term_wq([TF, false], VNs), nl))).

type_check_return_to_shen(T) :-
    bb_get('#%variable_names', VNs0),
    variable_labels(T, VNs0, VNs),
    executable_functor_shen_expr(T, TF),
    throw('#%return_success'((ipc:write_canonical_term_wq([[type_functor, TF]], VNs), nl))).

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

data_functor_shen_expr(T, T) :-
    (  atomic(T)
    ;  var(T)
    ),
    !.
data_functor_shen_expr([T|Ts], '.'('.', U, Us)) :-
    data_functor_shen_expr(T, U),
    data_functor_shen_expr(Ts, Us),
    !.
data_functor_shen_expr(T, '.'('.', F, Vs)) :-
    T =.. [F | Ts],
    data_functor_shen_expr(Ts, Vs).

list_dot_functor([], []).
list_dot_functor([T|Ts], '.'(T, Us)) :-
    list_dot_functor(Ts, Us).

executable_functor_shen_expr(T, T) :-
    (  atomic(T)
    ;  var(T)
    ),
    !.
executable_functor_shen_expr([T|Ts], '.'('.', U, Us)) :-
    executable_functor_shen_expr(T, U),
    executable_functor_shen_expr(Ts, Us),
    !.
executable_functor_shen_expr(T, '.'(F, Vs)) :-
    T =.. [F | Ts],
    maplist(executable_functor_shen_expr, Ts, Us),
    list_dot_functor(Us, Vs).
