:- module(term_variables, [of_type/2,
                           op(1200, fx, -->),
                           term/1,
                           value_type/2]).

:- use_module(library(atts)).
:- use_module(library(dcgs)).
:- use_module(library(iso_ext)).
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

of_type(X, T) :-
    var(X),
    get_atts(X, of_types(Types)),
    member(T, Types).

value_type(X, T) :-
    (  var(X) ->
       (  get_atts(X, of_types(Types)) ->
          (  member(T, Types) ->
             true
          ;  put_atts(X, of_types([T|Types]))
          )
       ;  put_atts(X, of_types([T]))
       )
    ;  T = symbol,
       atom(X) ->
       true
    ;  T = number,
       number(X) ->
       true
    ;  T = string,
       partial_string(X),
       partial_string_tail(X, []) ->
       true
    ).

term(Term) :-
    term_variables(Term, TVs),
    maplist(\X^put_atts(X, term_var), TVs).
