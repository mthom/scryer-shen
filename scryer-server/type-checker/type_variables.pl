:- module(type_variables, [exists/1, type/1, type_eq/2]).

:- use_module(library(atts)).
:- use_module(library(lambda)).
:- use_module(library(lists)).

:- attribute type_equated/0, type_var/0.

verify_attributes(Var, Other, []) :-
    (  get_atts(Var, type_equated) ->
       true
    ;  var(Other),
       (  get_atts(Other, type_equated) ->
          true
       ;  get_atts(Other, type_var) ->
          (  get_atts(Var, type_var),
             Var == Other
          )
       ;  true
       )
    ).

type(T) :-
    term_variables(T, TVs),
    maplist(\TV^put_atts(TV, type_var), TVs).

type_eq(T, U) :-
    term_variables([T, U], TVs),
    maplist(\TV^put_atts(TV, [type_var, type_equated]), TVs),
    T = U,
    maplist(\TV^(  var(TV) ->
                   put_atts(TV, -type_equated)
                ;  true
                ),
            TVs).

exists(T) :-
    (  var(T) ->
       (  get_atts(T, type_var) ->
          true
       ;  put_atts(T, type_equated)
       )
    ;  true
    ).
