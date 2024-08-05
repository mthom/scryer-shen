:- module(proof_trees, [verify_proof_tree/1]).

:- use_module(library(assoc)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets), [ord_subset/2]).

verify_proof_tree(Tree) :-
    phrase(tree_variable_nodes(Tree, _MarkedTree), ProofTerms),
    empty_assoc(EmptyTree),
    proved_tree_list(EmptyTree, ProvenTree, ProofTerms),
    maplist(check_pending_proof(ProvenTree), ProofTerms).

check_pending_proof(ProvenTree, pending(Hyps, G)) :-
    get_assoc(G, ProvenTree, HypsList),
    member(CHyps, HypsList),
    ord_subset(Hyps, CHyps).
check_pending_proof(_ProvenTree, proved(_Hyps, _G)).

proved_tree_list(FinalTree, FinalTree, []).
proved_tree_list(ProvenTree0, FinalTree, [T|Ts]) :-
    populate_proved_tree(ProvenTree0, ProvenTree1, T),
    proved_tree_list(ProvenTree1, FinalTree, Ts).

get_assoc_or_default(Key, Tree, [Value|Values]) :-
    get_assoc(Key, Tree, [Value|Values]).
get_assoc_or_default(_Key, _Tree, []).

populate_proved_tree(ProvenTree0, ProvenTree1, proved(Hyps, G)) :-
    get_assoc_or_default(G, ProvenTree0, CurrentHyps),
    put_assoc(G, ProvenTree0, [Hyps|CurrentHyps], ProvenTree1).
populate_proved_tree(ProvenTree, ProvenTree, pending(_Hyps, _G)).

tree_variable_nodes(OldTree, NewTree) -->
    tree_variable_nodes([], OldTree, NewTree).

tree_variable_nodes(Hyps0, OldTree, NewTree) -->
    { sub_nodes(TreeFunctor, RootGoal, Goals, OldTree),
      append_hyps(Hyps0, RootGoal, Hyps1) },
    foldl(tree_variable_nodes(Hyps1), Goals, SucceedingGoals),
    succ_tree_nodes(Hyps1, TreeFunctor, RootGoal, SucceedingGoals, NewTree).

sub_nodes(t, RootGoal, Goals, OldTree) :-
    OldTree =.. [t, RootGoal | Goals].
sub_nodes(fix_point, RootGoal, [], fix_point(RootGoal)).
sub_nodes(assumed, RootGoal, [], assumed(RootGoal)).
sub_nodes(discharged, RootGoal, [], discharged(RootGoal)).

append_hyps(Hyps0, discharged(Hyp), Hyps1) :-
    sort([Hyp|Hyps0], Hyps1).
append_hyps(Hyps, g(_), Hyps).

succ_tree_nodes(Hyps, t, RootGoal, Goals, NewTree) -->
    { NewTree =.. [t, RootGoal, ProofStatus | Goals] },
    goals_proof_status(Hyps, Goals, ProofStatus).
succ_tree_nodes(Hyps, fix_point, RootGoal, [], fix_point(RootGoal)) -->
    [pending(Hyps, RootGoal)].
succ_tree_nodes(Hyps, assumed, RootGoal, [], assumed(RootGoal)) -->
    [proved(Hyps, RootGoal)].
succ_tree_nodes(Hyps, discharged, RootGoal, [], discharged(RootGoal)) -->
    [discharged(Hyps, RootGoal)].

goals_proof_status(_Hyps, [], proved) -->
    !.
goals_proof_status(_Hyps, [], pending) -->
    [].
goals_proof_status(Hyps, [G|Gs], ProofStatus) -->
    goal_proof_status(Hyps, G, ProofStatus),
    goals_proof_status(Hyps, Gs, ProofStatus).

sub_proof_propagation(Hyps, pending, pending, G) -->
    [pending(Hyps, G)].
sub_proof_propagation(Hyps, proved, _ProofStatus, G) -->
    [proved(Hyps, G)].

goal_proof_status(Hyps, Node, ProofStatus) -->
    { Node =.. [t, g(G), SubProofStatus | _] },
    sub_proof_propagation(Hyps, SubProofStatus, ProofStatus, G).
goal_proof_status(Hyps0, t(discharged(Hyp), SubProofStatus, SubNode), ProofStatus) -->
    { SubNode =.. [t, g(G) | _],
      append_hyps(Hyps0, discharged(Hyp), Hyps) },
    sub_proof_propagation(Hyps, SubProofStatus, ProofStatus, G).
goal_proof_status(Hyps, fix_point(G), pending) -->
    [pending(Hyps, G)].
goal_proof_status(_Hyps, assumed(_G), _) -->
    [].
