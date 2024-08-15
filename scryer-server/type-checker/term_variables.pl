:- module(term_variables, [of_type/2,
                           op(1200, fx, -->),
                           op(599, fx, '#%?'),
                           term/1,
                           value_type/2]).

:- use_module(library(atts)).
:- use_module(library(dcgs)).
:- use_module(library(lambda)).
:- use_module(library(lists)).

:- attribute term_var/0, of_types/1.

verify_attributes(Var, Other, []) :-
    (  var(Other) ->
       (  get_atts(Other, term_var) ->
          (  get_atts(Var, term_var) ->
             Var == Other
          ;  true
          )
       ;  true
       )
    ;  false
    ).

unwrap_literal('#%?'(A), A).
unwrap_literal('#%number'(A), A).
unwrap_literal('#%string'(A), A).
unwrap_literal('#%symbol'(A), A).

of_type(Lit, T) :-
    unwrap_literal(Lit, X),
    var(X),
    get_atts(X, of_types(Types)),
    member(T, Types).

value_type(Lit, T) :-
    (  unwrap_literal(Lit, X),
       var(X),
       get_atts(X, of_types(Types)),
       (  member(T, Types) ->
          true
       ;  put_atts(X, of_types([T|Types]))
       )
    ;  true
    ).

term(Term) :-
    term_variables(Term, TVs),
    maplist(\X^put_atts(X, term_var), TVs).
