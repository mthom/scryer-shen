:- module(inference_rules, [type_check//2,
                            provable//1,
                            declare/2]).

:- use_module(term_variables).

:- discontiguous(type_check/4).
:- multifile(type_check/4).

type_check(? X, T) -->
    { of_type(? X, T) }.
type_check([], list(_A)) --> [].
type_check(true, boolean) --> [].
type_check(false, boolean) --> [].
type_check(string(X), string) -->
    { value_type(string(X), string) }.
type_check(number(X), number) -->
    { value_type(number(X), number) }.
type_check(symbol(X), symbol) -->
    { value_type(symbol(X), symbol) }.
type_check(symbol(F), FnT) -->
    [g(declare(F, FnT))].
type_check([X|Xs], list(A)) -->
    [g(type_check(X, A)),
     g(type_check(Xs, list(A)))].
type_check('@p'(X,Y), A * B) -->
    [g(type_check(X, A)),
     g(type_check(Y, B))].
type_check('@s'(X,Y), string) -->
    [g(type_check(X, string)),
     g(type_check(Y, string))].
type_check('@v'(X, Y), vector(A)) -->
    [g(type_check(X, A)),
     g(type_check(Y, vector(A)))].
type_check(<>, vector(_A)) -->
    [].
type_check(/.(Var, Body), (A --> B)) -->
    [h(exists(A)),
     h(type_check(Var, A)),
     g(type_check(Body, B))].
type_check(let(Var, Binding, Body), A) -->
    [g(type_check(Binding, B)),
     h(type_check(Var, B)),
     g(type_check(Body, A))].
type_check(if(Condition, TrueBranch, FalseBranch), A) -->
    [g(type_check(Condition, boolean)),
     g(type_check(TrueBranch, A)),
     g(type_check(FalseBranch, A))].
type_check(apply(F, X), B) -->
    [g(type_check(F, (A --> B))),
     g(type_check(X, A))].
type_check(apply(F), R) -->
    [g(type_check(F, -->(R)))].
type_check(fn(F), A) -->
    [g(declare(F, A))].


:- discontiguous(provable/3).
:- multifile(provable/3).

provable(type_check([X|Xs], list(A))) -->
    [h(type_check(X, A)),
     h(type_check(Xs, list(A)))].
provable(type_check('@p'(X, Y), A * B)) -->
    [h(type_check(X, A)),
     h(type_check(Y, B))].
provable(type_check('@s'(X, Y), string)) -->
    [h(type_check(X, string)),
     h(type_check(Y, string))].
provable(type_check('@v'(X, Y), vector(A))) -->
    [h(type_check(X, A)),
     h(type_check(Y, vector(A)))].


:- dynamic(declare/2).

declare(load, (string --> symbol)).
declare(tc, (symbol --> boolean)).
declare(+, (number --> (number --> number))).
declare(-, (number --> (number --> number))).
declare(*, (number --> (number --> number))).
declare(/, (number --> (number --> number))).
declare(hd, (list(A) --> A)).
declare(tl, (list(A) --> list(A))).
declare(map, ((A --> B) --> (list(A) --> list(B)))).
