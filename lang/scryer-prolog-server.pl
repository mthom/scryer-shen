:- use_module(library(cont)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).

:- meta_predicate shen_prolog_eval(0).

shen_prolog_eval(Query) :-
    reset(Query, Ball, Cont),
    (  var(Ball),
       !,
       write([true, false])
    ;  pause_or_return(Cont, Ball)
    ),
    !.
shen_prolog_eval(_) :-
    write([false, false]).

pause_or_return(none, Ball) :-
    function_eval(Ball).
pause_or_return(cont(Cont), Ball) :-
    function_eval(Ball),
    shen_prolog_eval(Cont).

function_eval(bind(F, X)) :-
    write_term_to_sexpr(F, SExpr),
    write([SExpr, true]), % write to scryer-shen which is listening on stdout ...
    read(X).      % .. and block until the result is read back from scryer-shen.
function_eval(return_to_shen(T)) :-
    write_term_to_sexpr(T, SExpr),
    write([SExpr, false]).

write_term_to_sexpr(T, SExpr) :-
    (   var(T) -> T = SExpr
    ;   T =.. [TF | TArgs],
        write_functor_to_sexpr(TF, TArgs, SExpr)
    ).

write_functor_to_sexpr('.', [Car, Cdr], SExpr) :-
    !,
    write_functor_to_sexpr(cons, [Car, Cdr], SExpr).
write_functor_to_sexpr(TF, TArgs, SExpr) :-
    atom(TF),
    maplist(write_term_to_sexpr, TArgs, ArgSExprs),
    phrase(format_("(~w ", [TF]), SExpr, SExprRest),
    foldl(write_term_to_sexpr, ArgSExprs, SExprRest, ")")

write_term_arg_to_sexpr(SExprArg, SExprHead, SExprTail) :-
    phrase(format_("~w ", [SExprArg]), SExprHead, SExprTail).
