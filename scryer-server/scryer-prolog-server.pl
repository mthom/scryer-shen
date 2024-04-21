:- module('$scryer-prolog-server', [shen_prolog_eval/2]).

:- use_module('scryer-shen-ipc').

:- meta_predicate shen_prolog_eval(0, ?).

shen_prolog_eval(Query, VNs) :-
    catch(continue(Query, VNs),
          E,
          write_error_to_sexpr(E)).
