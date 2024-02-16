:- module('$scryer-shen-toplevel', []).

:- use_module(library(charsio)).
:- use_module(library(cont)).
:- use_module(library(error)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).

:- use_module('scryer-prolog-server').

repl :-
    catch(read_and_match, E, print_exception(E)),
    false.
repl :-
    repl.

read_and_match :-
    '$read_query_term'(_, Term, _, _, VarList),
    instruction_match(Term, VarList).

instruction_match(Term, VarList) :-
    (  var(Term) ->
       throw(error(instantiation_error, repl/0))
    ;  Term = [Item] ->
       (  atom(Item) ->
          (  Item == user ->
             catch(load(user_input), E, print_exception_with_check(E))
          ;
             consult(Item)
          )
       ;  catch(type_error(atom, Item, repl/0),
                E,
                print_exception_with_check(E))
       )
    ;  Term = end_of_file ->
       halt
    ;  Term = shen_prolog_eval(Query) ->
       expand_goal(Query, user, Query0),
       shen_prolog_eval(user:Query0, VarList)
    ).

print_exception(E) :-
    (  E == error('$interrupt_thrown', repl) -> nl
    ;  true
    ),
    loader:write_error(E),
    nl.

print_exception_with_check(E) :-
    (  E = error(_, _:_) -> true
    ;  print_exception(E)
    ).
