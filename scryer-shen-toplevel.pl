:- module('$scryer-shen-toplevel', []).

:- use_module(library(charsio)).
:- use_module(library(error)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).

repl :-
    catch(read_and_match, E, print_exception(E)),
    false. %% this is for GC, until we get actual GC.
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
    ;  expand_goal(Term, user, Term0),
       call(user:Term0, VarList)
    ).

print_exception(E) :-
    (  E == error('$interrupt_thrown', repl) -> nl % print the
                                                   % exception on a
                                                   % newline to evade
                                                   % "^C".
    ;  true
    ),
    loader:write_error(E),
    nl.

print_exception_with_check(E) :-
    (  E = error(_, _:_) -> true % if the error source contains a line
    % number, a GNU-style error message
    % is expected to be printed instead.
    ;  print_exception(E)
    ).


:- initialization(repl).
