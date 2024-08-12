:- module(inference_rules, [type_check//2,
                            provable//1,
                            declare/2]).

:- use_module(term_variables).

:- discontiguous(type_check/4).
:- multifile(type_check/4).

type_check('#%?' X, T) -->
    { of_type(? X, T) }.
type_check([], list(_A)) --> [].
type_check(true, boolean) --> [].
type_check(false, boolean) --> [].
type_check('#%string'(X), string) -->
    { value_type('#%string'(X), string) }.
type_check('#%number'(X), number) -->
    { value_type('#%number'(X), number) }.
type_check('#%symbol'(X), symbol) -->
    { value_type('#%symbol'(X), symbol) }.
type_check('#%symbol'(F), FnT) -->
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
     h(exists(Var, B)),
     g(type_check(Body, A))].
type_check(if(Condition, TrueBranch, FalseBranch), A) -->
    [g(type_check(Condition, boolean)),
     g(type_check(TrueBranch, A)),
     g(type_check(FalseBranch, A))].
type_check('#%apply'(F, X), B) -->
    [g(type_check(F, (A --> B))),
     g(type_check(X, A))].
type_check('#%apply'(F), R) -->
    [g(type_check(F, -->(R)))].
type_check(fn(F), A) -->
    [g(declare(F, A))].
type_check(freeze(E), lazy(A)) -->
    [g(type_check(E, A))].
type_check(thaw(E), A) -->
    [g(type_check(E, lazy(A)))].


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
declare(=, (A --> (A --> boolean))).
declare(==, (A --> (A --> boolean))).
declare(>, (number --> (number --> boolean))).
declare(<, (number --> (number --> boolean))).
declare(>=, (number --> (number --> boolean))).
declare(<=, (number --> (number --> boolean))).
declare(head, (list(A) --> A)).
declare(tail, (list(A) --> list(A))).
declare(explode, (_A --> list(string))).
declare(cd, (string --> string)).
declare(arity, (_A --> number)).
declare('absvector?', (_A --> boolean)).
declare('bound?', (symbol --> boolean)).
declare(cons, (A --> (list(A) --> list(A)))).
declare(adjoin, (A --> (list(A) --> list(A)))).
declare(append, (list(A) --> (list(A) --> list(A)))).
declare(difference, (list(A) --> (list(A) --> list(A)))).
declare('element?', (A --> (list(A) --> boolean))).
declare(map, ((A --> B) --> (list(A) --> list(B)))).
declare(cn, (string --> (string --> string))).
declare('cons?', (_A --> boolean)).
declare('empty?', (_A --> boolean)).
declare('error-to-string', (exception --> string)).
declare(external, (symbol --> list(symbol))).
declare(internal, (symbol --> list(symbol))).
declare(limit, (vector(_A) --> number)).
declare('symbol?', (_A --> boolean)).
declare('atom?', (_A --> boolean)).
declare('integer?', (_A --> boolean)).
declare(systemf, (symbol --> symbol)).
declare(freeze, (A --> lazy(A))).
declare(thaw, (lazy(A) --> A)).
declare(undefmacro, (symbol --> symbol)).
declare(vector, (number --> vector(_A))).
declare('<-vector', (vector(A) --> (number --> A))).
declare('vector->', (vector(A) --> (number --> (A --> vector(A))))).
declare(and, (boolean --> (boolean --> boolean))).
declare(or, (boolean --> (boolean --> boolean))).
declare(fail, -->(symbol)).
declare(fix, ((A --> A) --> (A --> A))).
declare(fst, ((A * _B) --> A)).
declare(snd, ((_A * B) --> B)).
declare(gensym, (symbol --> symbol)).
declare(hdstr, (string --> string)).
declare(intern, (string --> symbol)).
