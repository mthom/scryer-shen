:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(cont)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lambda)).
:- use_module(library(lists)).

:- meta_predicate shen_prolog_eval(0, ?).

% the goal called by Scryer Shen is shen_prolog_eval/1, so to allow
% goal expansion to occur in the first argument of shen_prolog_eval/2,
% declare the single argument version a meta-predicate even though it
% is not defined anywhere.

:- meta_predicate shen_prolog_eval(0).

shen_prolog_eval(Query, VNs) :-
    catch(shen_prolog_eval_(Query, VNs),
          E,
          write_error_to_sexpr(E)).

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

provisional_variable_labels(TermVars, NumVars0, Min, VNs) :-
    NumVars is NumVars0 + Min - 1,
    numlist(Min, NumVars, VarLabels),
    maplist(\V^N^VarForm^(
                write_term_to_chars('$VAR'(N), [numbervars(true)], VCs),
                atom_chars(VA, VCs),
                VarForm = (VA = V)
            ),
            TermVars,
            VarLabels,
            VNs
           ).

matching_variable_labels_loop(TermVars, NumVars0, Min, VNs0, VNs) :-
    provisional_variable_labels(TermVars, NumVars0, Min, VNs1),
    label_matches(VNs0, VNs1, Matches, Min),
    (   Matches == [] ->
        append(VNs0, VNs1, VNs)
    ;   list_max(Matches, Max),
        Max1 is Max + 1,
        matching_variable_labels_loop(TermVars, NumVars0, Max1, VNs0, VNs)
    ).

variable_labels(Term, VNs0, VNs) :-
    term_variables(Term, TermVars),
    length(TermVars, NumVars0),
    matching_variable_labels_loop(TermVars, NumVars0, 1, VNs0, VNs).

write_error_to_sexpr(error(Error, Src)) :-
    format("[(error \"error(~w, ~w)\") false]~n", [Error, Src]),
    false.

shen_prolog_eval_(Query, VNs) :-
    reset(Query, Ball, Cont),
    (  var(Ball) ->
       format("[true false]~n", [])
    ;  pause_or_return(Cont, Ball, VNs)
    ),
    !.
shen_prolog_eval_(_, _) :-
    format("[false false]~n", []).

pause_or_return(none, Ball, VNs) :-
    function_eval(Ball, VNs).
pause_or_return(cont(Cont), bind(F,X), VNs) :-
    !,
    function_eval(bind(F,X), VNs),
    shen_prolog_eval_(Cont, VNs).
pause_or_return(cont(_Cont), return_to_shen(T), VNs0) :-
    variable_labels(T, VNs0, VNs),
    function_eval(return_to_shen(T), VNs).

function_eval(bind(F,X), VNs) :-
    functor_sexpr(VNs, "(~w ", F, SExpr),
    format("[~s true]~n", [SExpr]), % write to scryer-shen which is listening to stdout ...
    read(X).                        % .. and block until the result is read back from scryer-shen.
function_eval(return_to_shen(T), VNs) :-
    functor_sexpr(VNs, "[~w ", T, SExpr),
    format("[~s false]~n", [SExpr]).

functor_sexpr(VNs, FunctorRep, T, SExpr) :-
    (   var(T) ->
        write_term_to_chars(T, [variable_names(VNs)], SExpr)
    ;   partial_string(T),
        partial_string_tail(T, []) ->
        phrase(format_("\"~s\"", [T]), SExpr)
    ;   T =.. [TF | TArgs],
        functor_args_sexpr(VNs, FunctorRep, TF, TArgs, SExpr)
    ).

end_bracket("(~w ", ")").
end_bracket("[~w ", "]").

functor_args_sexpr(VNs, FunctorRep, '.', [Car, Cdr], SExpr) :-
    !,
    functor_sexpr(VNs, FunctorRep, Car, CarSExpr),
    functor_sexpr(VNs, FunctorRep, Cdr, CdrSExpr),
    phrase(format_("[~s | ~s]", [CarSExpr, CdrSExpr]), SExpr).
functor_args_sexpr(_VNs, _FunctorRep, TF, [], SExpr) :-
    phrase(format_("~w", [TF]), SExpr).
functor_args_sexpr(VNs, FunctorRep, TF, [TArg|TArgs], SExpr) :-
    maplist(functor_sexpr(VNs, FunctorRep), [TArg|TArgs], ArgSExprs),
    phrase(format_(FunctorRep, [TF]), SExpr, SExprRest),
    end_bracket(FunctorRep, EndBracket),
    foldl(functor_arg_sexpr, ArgSExprs, SExprRest, EndBracket).

functor_arg_sexpr(SExprArg, SExprHead, SExprTail) :-
    phrase(format_("~s ", [SExprArg]), SExprHead, SExprTail).
