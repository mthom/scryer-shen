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

write_error_to_sexpr(error(Error, Src)) :-
    format("[(error \"error(~w, ~w)\") false]~n", [Error, Src]),
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
    !,
    function_eval(bind(F,X)),
    shen_prolog_eval(Cont).
pause_or_return(cont(_Cont), return_to_shen(T)) :-
    function_eval(return_to_shen(T)).

function_eval(bind(F,X)) :-
    functor_sexpr("(~w ", F, SExpr),
    format("[~s true]~n", [SExpr]), % write to scryer-shen which is listening to stdout ...
    read(X).                        % .. and block until the result is read back from scryer-shen.
function_eval(return_to_shen(T)) :-
    functor_sexpr("[~w ", T, SExpr),
    format("[~s false]~n", [SExpr]).

functor_sexpr(FunctorRep, T, SExpr) :-
    (   var(T) ->
        phrase(format_("~w", [T]), SExpr)
    ;   partial_string(T),
        partial_string_tail(T, []) ->
        phrase(format_("\"~s\"", [T]), SExpr)
    ;   T =.. [TF | TArgs],
        functor_args_sexpr(FunctorRep, TF, TArgs, SExpr)
    ).

end_bracket("(~w ", ")").
end_bracket("[~w ", "]").

functor_args_sexpr(FunctorRep, '.', [Car, Cdr], SExpr) :-
    !,
    functor_sexpr(FunctorRep, Car, CarSExpr),
    functor_sexpr(FunctorRep, Cdr, CdrSExpr),
    phrase(format_("[cons ~s ~s]", [CarSExpr, CdrSExpr]), SExpr).
functor_args_sexpr(_FunctorRep, TF, [], SExpr) :-
    phrase(format_("~w", [TF]), SExpr).
functor_args_sexpr(FunctorRep, TF, [TArg|TArgs], SExpr) :-
    maplist(functor_sexpr(FunctorRep), [TArg|TArgs], ArgSExprs),
    phrase(format_(FunctorRep, [TF]), SExpr, SExprRest),
    end_bracket(FunctorRep, EndBracket),
    foldl(functor_arg_sexpr, ArgSExprs, SExprRest, EndBracket).

functor_arg_sexpr(SExprArg, SExprHead, SExprTail) :-
    phrase(format_("~s ", [SExprArg]), SExprHead, SExprTail).
