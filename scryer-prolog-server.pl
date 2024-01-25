:- use_module(library(cont)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).

:- meta_predicate shen_prolog_eval(0).

shen_prolog_eval(Query) :-
    catch(shen_prolog_eval_(Query),
          E,
          write_error_to_sexpr(E)).

write_error_to_sexpr(error(Error)) :-
    format("[(error \"~w\") false]~n", [Error]),
    false.

shen_prolog_eval_(Query) :-
    reset(Query, Ball, Cont),
    (  var(Ball),
       !,
       format("[true false]~n", [])
    ;  pause_or_return(Cont, Ball)
    ),
    !.
shen_prolog_eval_(_) :-
    format("[false false]~n", []).

pause_or_return(none, Ball) :-
    function_eval(Ball).
pause_or_return(cont(Cont), bind(F,X)) :-
    function_eval(bind(F,X)),
    shen_prolog_eval(Cont).
pause_or_return(cont(_Cont), return_to_shen(T)) :-
    function_eval(return_to_shen(T)).

function_eval(bind(F,X)) :-
    write_term_to_sexpr(F, SExpr),
    format("[~s true]~n", [SExpr]), % write to scryer-shen which is listening to stdout ...
    read(X).                        % .. and block until the result is read back from scryer-shen.
function_eval(return_to_shen(T)) :-
    write_term_to_sexpr(T, SExpr),
    format("[~s false]~n", [SExpr]).

write_term_to_sexpr(T, SExpr) :-
    (   var(T) ->
        phrase(format_("~w", [T]), SExpr)
    ;   T =.. [TF | TArgs],
        write_functor_to_sexpr(TF, TArgs, SExpr)
    ).

write_functor_to_sexpr('.', [Car, Cdr], SExpr) :-
    !,
    (   partial_string([Car | Cdr], Str, []) ->
        phrase(format_("\"~s\"", [Str]), SExpr)
    ;   write_term_to_sexpr(Car, CarSExpr),
        write_term_to_sexpr(Cdr, CdrSExpr),
        phrase(format_("[cons ~s ~s]", [CarSExpr, CdrSExpr]), SExpr)
    ).
write_functor_to_sexpr(TF, [], SExpr) :-
    phrase(format_("~w", [TF]), SExpr).
write_functor_to_sexpr(TF, [TArg|TArgs], SExpr) :-
    maplist(write_term_to_sexpr, [TArg|TArgs], ArgSExprs),
    phrase(format_("(~w ", [TF]), SExpr, SExprRest),
    foldl(write_term_arg_to_sexpr, ArgSExprs, SExprRest, ")").

write_term_arg_to_sexpr(SExprArg, SExprHead, SExprTail) :-
    phrase(format_("~s ", [SExprArg]), SExprHead, SExprTail).
