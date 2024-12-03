/*
    Author:        Markus Triska
    E-mail:        triska@metalevel.at
    Copyright (C): 2014-2024 Markus Triska

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   CLP(B): Constraint Logic Programming over Boolean variables.

   This is like library(clpb) which ships with Scryer Prolog, except
   that we use Zero-suppressed Binary Decision Diagrams (ZDDs) here.

   ZDDs can save a lot of memory in situations where solutions have
   most Boolean variables equal to 0. This is the case in many
   covering tasks.

   This file is mostly a drop-in-replacement, with the following
   important difference:

      Before using sat/1, you MUST call zdd_set_vars/1 with a list of
      all Boolean variables that occur in your model.

   For example:

      ?- zdd_set_vars([X,Y]), sat(X+Y).

   yielding:

      sat(X=:=X),
      node(4)- (X->node(0);node(2)),
      node(2)- (Y->true;false),
      node(0)- (Y->true;true),
      sat(Y=:=Y).

   The current version is intended for tasks where library(clpb) runs
   out of memory, and mostly for counting solutions with sat_count/2.

   ATTENTION: sat_count/2 differs from the one in Scryer Prolog's
   library(clpb) if there are CLP(B) variables outside the expression!

   Current limitations:

       -) unification of CLP(B) variables is NOT yet implemented
       -) labeling/1 does NOT work yet
       -) card/2 does not yet support integer ranges

   Future versions may no longer require zdd_set_vars/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(clpb, [
                 op(300, fy, ~),
                 op(500, yfx, #),
                 zdd_set_vars/1,
                 sat/1,
                 sat_count/1,
                 taut/2,
                 labeling/1,
                 sat_count/2
                ]).

:- use_module(library(error), [domain_error/3, type_error/3]).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(pairs)).
:- use_module(library(dcgs)).
:- use_module(library(atts)).
:- use_module(library(iso_ext)).
:- use_module(library(debug)).

:- attribute
        clpb/1,
        clpb_bdd/1,
        clpb_hash/1,
        clpb_visited/1.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Compatibility predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

must_be(What, Term) :- must_be(What, unknown(Term)-1, Term).

must_be(acyclic, Where, Term) :- !,
        (   acyclic_term(Term) ->
            true
        ;   domain_error(acyclic_term, Term, Where)
        ).
must_be(list, Where, Term) :- !,
        (   acyclic_term(Term), clpz_list(Term, Where) -> true
        ;   type_error(list, Term, Where)
        ).
must_be(list(What), Where, Term) :- !,
        must_be(list, Where, Term),
        maplist(must_be(What, Where), Term).
must_be(ground, _, Term) :- !,
        functor(Term, _, _).

must_be(Type, _, Term) :-
        error:must_be(Type, Term).

clpz_list(Nil, _) :- Nil == [].
clpz_list(Ls, Where) :-
    (   var(Ls) ->
        instantiation_error(Ls, Where)
    ;   Ls = [_|Rest],
        clpz_list(Rest, Where)
    ).


instantiation_error(Term) :- instantiation_error(Term, unknown(Term)-1).

instantiation_error(_, Goal-Arg) :-
	throw(error(instantiation_error, instantiation_error(Goal, Arg))).


domain_error(Expectation, Term) :-
        domain_error(Expectation, Term, unknown(Term)-1).


type_error(Expectation, Term) :-
        type_error(Expectation, Term, unknown(Term)-1).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compatibility predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- meta_predicate(include(1, ?, ?)).

include(_, [], []).
include(Goal, [L|Ls0], Ls) :-
        (   call(Goal, L) ->
            Ls = [L|Rest]
        ;   Ls = Rest
        ),
        include(Goal, Ls0, Rest).

goal_expansion(get_attr(Var, Module, Value), (var(Var),get_atts(Var, Access))) :-
        Access =.. [Module,Value].

goal_expansion(put_attr(Var, Module, Value), put_atts(Var, Access)) :-
        Access =.. [Module,Value].

goal_expansion(del_attr(Var, Module), (var(Var) -> put_atts(Var, -Access);true)) :-
        Access =.. [Module,_].


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Each CLP(B) variable belongs to exactly one BDD. Each CLP(B)
   variable gets an attribute (in module "clpb") of the form:

        index_root(Index,Root)

   where Index is the variable's unique integer index, and Root is the
   root of the BDD that the variable belongs to.

   Each CLP(B) variable also gets an attribute in module clpb_hash: an
   association table node(LID,HID) -> Node, to keep the BDD reduced.
   The association table of each variable must be rebuilt on occasion
   to remove nodes that are no longer reachable. We rebuild the
   association tables of involved variables after BDDs are merged to
   build a new root. This only serves to reclaim memory: Keeping a
   node in a local table even when it no longer occurs in any BDD does
   not affect the solver's correctness.

   A root is a logical variable with a single attribute ("clpb_bdd")
   of the form:

        Sat-BDD

   where Sat is the SAT formula (in original form) that corresponds to
   BDD. Sat is necessary to rebuild the BDD after variable aliasing.

   Finally, a BDD is either:

      *)  The integers 0 or 1, denoting false and true, respectively, or
      *)  A node of the form

           node(ID, Var, Low, High, Aux)
               Where ID is the node's unique integer ID, Var is the
               node's branching variable, and Low and High are the
               node's low (Var = 0) and high (Var = 1) children. Aux
               is a free variable, one for each node, that can be used
               to attach attributes and store intermediate results.

   Variable aliasing is treated as a conjunction of corresponding SAT
   formulae.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Type checking.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

is_sat(V)     :- var(V), !.
is_sat(I)     :- integer(I), between(0, 1, I).
is_sat(~A)    :- is_sat(A).
is_sat(A*B)   :- is_sat(A), is_sat(B).
is_sat(A+B)   :- is_sat(A), is_sat(B).
is_sat(A#B)   :- is_sat(A), is_sat(B).
is_sat(A=:=B) :- is_sat(A), is_sat(B).
is_sat(A=\=B) :- is_sat(A), is_sat(B).
is_sat(A=<B)  :- is_sat(A), is_sat(B).
is_sat(A>=B)  :- is_sat(A), is_sat(B).
is_sat(A<B)   :- is_sat(A), is_sat(B).
is_sat(A>B)   :- is_sat(A), is_sat(B).
is_sat(+(Ls)) :- must_be(list, Ls), maplist(is_sat, Ls).
is_sat(*(Ls)) :- must_be(list, Ls), maplist(is_sat, Ls).
is_sat(X^F)   :- var(X), is_sat(F).
is_sat(card(Is,Fs)) :-
        must_be(list(ground), Is),
        must_be(list, Fs),
        maplist(is_sat, Fs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Rewriting to canonical expressions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% elementary
sat_rewrite(V, V)       :- var(V), !.
sat_rewrite(I, I)       :- integer(I).
sat_rewrite(P0*Q0, P*Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(P0+Q0, P+Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(P0#Q0, P#Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(X^F0, X^F)  :- sat_rewrite(F0, F).
sat_rewrite(card(Is,Fs0), card(Is,Fs)) :-
        maplist(sat_rewrite, Fs0, Fs).
% synonyms
sat_rewrite(~P, R)      :- sat_rewrite(1 # P, R).
sat_rewrite(P =:= Q, R) :- sat_rewrite(~P # Q, R).
sat_rewrite(P =\= Q, R) :- sat_rewrite(P # Q, R).
sat_rewrite(P =< Q, R)  :- sat_rewrite(~P + Q, R).
sat_rewrite(P >= Q, R)  :- sat_rewrite(Q =< P, R).
sat_rewrite(P < Q, R)   :- sat_rewrite(~P * Q, R).
sat_rewrite(P > Q, R)   :- sat_rewrite(Q < P, R).
sat_rewrite(+(Ls), R)   :- foldl(or, Ls, 0, F), sat_rewrite(F, R).
sat_rewrite(*(Ls), R)   :- foldl(and, Ls, 1, F), sat_rewrite(F, R).

or(A, B, B + A).

and(A, B, B * A).

must_be_sat(Sat) :-
        (   is_sat(Sat) -> true
        ;   no_truth_value(Sat)
        ).

no_truth_value(Term) :- domain_error(clpb_expr, Term).

parse_sat(Sat0, Sat) :-
        must_be_sat(Sat0),
        sat_rewrite(Sat0, Sat),
        term_variables(Sat, Vs),
        maplist(enumerate_variable, Vs).

enumerate_variable(V) :-
        (   var_index_root(V, _, _) -> true
        ;   clpb_next_id('$clpb_next_var', Index),
            put_attr(V, clpb, index_root(Index,_)),
            put_empty_hash(V)
        ).

var_index(V, I) :- var_index_root(V, I, _).

var_index_root(V, I, Root) :- get_attr(V, clpb, index_root(I,Root)).

put_empty_hash(V) :-
        empty_assoc(H0),
        put_attr(V, clpb_hash, H0).

sat_roots(Sat, Roots) :-
        term_variables(Sat, Vs),
        maplist(var_index_root, Vs, _, Roots0),
        term_variables(Roots0, Roots).

%% sat(+Expr) is semidet.
%
% True iff Expr is a satisfiable Boolean expression.

sat(Sat0) :-
        (   phrase(sat_ands(Sat0), Ands), Ands = [_,_|_] ->
            maplist(sat, Ands)
        ;   parse_sat(Sat0, Sat),
            sat_bdd(Sat, BDD),
            sat_roots(Sat, Roots),
            roots_and(Roots, Sat0-BDD, And-BDD1),
            maplist(del_bdd, Roots),
            maplist(=(Root), Roots),
            root_put_formula_bdd(Root, And, BDD1),
            satisfiable_bdd(BDD1)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Posting many small sat/1 constraints is better than posting a huge
   conjunction (or negated disjunction), because unneeded nodes are
   removed from node tables after BDDs are merged. This is not
   possible in sat_bdd/2 because the nodes may occur in other BDDs. A
   better version of sat_bdd/2 or a proper implementation of a unique
   table including garbage collection would make this obsolete and
   also improve taut/2 and sat_count/2 in such cases.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

sat_ands(X) -->
        (   { var(X) } -> [X]
        ;   { X = (A*B) } -> sat_ands(A), sat_ands(B)
        ;   { X = *(Ls) } -> sat_ands_(Ls)
        ;   { X = ~Y } -> not_ors(Y)
        ;   [X]
        ).

sat_ands_([]) --> [].
sat_ands_([L|Ls]) --> [L], sat_ands_(Ls).

not_ors(X) -->
        (   { var(X) } -> [~X]
        ;   { X = (A+B) } -> not_ors(A), not_ors(B)
        ;   { X = +(Ls) } -> not_ors_(Ls)
        ;   [~X]
        ).

not_ors_([]) --> [].
not_ors_([L|Ls]) --> [~L], not_ors_(Ls).

del_bdd(Root) :- del_attr(Root, clpb_bdd).

root_get_formula_bdd(Root, F, BDD) :- get_attr(Root, clpb_bdd, F-BDD).

root_put_formula_bdd(Root, F, BDD) :- put_attr(Root, clpb_bdd, F-BDD).

roots_and(Roots, Sat0-BDD0, Sat-BDD) :-
        foldl(root_and, Roots, Sat0-BDD0, Sat-BDD),
        rebuild_hashes(BDD).

root_and(Root, Sat0-BDD0, Sat-BDD) :-
        (   root_get_formula_bdd(Root, F, B) ->
            Sat = F*Sat0,
            bdd_and(B, BDD0, BDD)
        ;   Sat = Sat0,
            BDD = BDD0
        ).

bdd_and(NA, NB, And) :-
        apply(*, NA, NB, And),
        is_bdd(And).

%% taut(+Expr, -T) is semidet
%
% Succeeds with T = 0 if the Boolean expression Expr cannot be
% satisfied, and with T = 1 if Expr is always true with respect to the
% current constraints. Fails otherwise.

taut(Sat0, T) :-
        parse_sat(Sat0, Sat),
        sat_roots(Sat, Roots),
        catch((roots_and(Roots, _-1, _-Ands),
               (   T = 0, unsatisfiable_conjunction(Sat, Ands) -> true
               ;   T = 1, unsatisfiable_conjunction(1#Sat, Ands) -> true
               ;   false
               ),
               % reset all attributes
               throw(truth(T))),
              truth(T),
              true).

unsatisfiable_conjunction(Sat, Ands) :-
        sat_bdd(Sat, BDD),
        bdd_and(BDD, Ands, B),
        B == 0.

satisfiable_bdd(BDD) :-
        (   BDD == 0 -> false
        ;   not_occurring_are_zero(BDD)
        ).

not_occurring_are_zero(BDD) :-
        all_variables_in_index_order(AllVs),
        bdd_variables(BDD, Vs),
        maplist(put_visited, Vs),
        maplist(not_visited_zero, AllVs, _Zs),
        maplist(unvisit, Vs).
        % AllVs = Zs.  % unification not yet supported

not_visited_zero(Var, Zero) :-
        (   is_visited(Var) -> Zero = _Any
        ;   Var \== 1,
            Zero = 0
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Node management. Always use an existing node, if there is one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_node(Var, Low, High, Node) :-
        (   High == 0 -> Node = Low
        ;   low_high_key(Low, High, Key),
            (   lookup_node(Var, Key, Node) -> true
            ;   clpb_next_id('$clpb_next_node', ID),
                Node = node(ID,Var,Low,High,_Aux),
                register_node(Var, Key, Node)
            )
        ).

make_node(Var, Low, High, Node) -->
        % make it conveniently usable within DCGs
        { make_node(Var, Low, High, Node) }.


low_high_key(Low, High, node(LID,HID)) :-
        node_id(Low, LID),
        node_id(High, HID).


rebuild_hashes(BDD) :-
        bdd_nodes(put_empty_hash, BDD, Nodes),
        maplist(re_register_node, Nodes).

re_register_node(Node) :-
        node_var_low_high(Node, Var, Low, High),
        low_high_key(Low, High, Key),
        register_node(Var, Key, Node).

register_node(Var, Key, Node) :-
        get_attr(Var, clpb_hash, H0),
        put_assoc(Key, H0, Node, H),
        put_attr(Var, clpb_hash, H).

lookup_node(Var, Key, Node) :-
        get_attr(Var, clpb_hash, H0),
        get_assoc(Key, H0, Node).


node_id(0, false).
node_id(1, true).
node_id(node(ID,_,_,_,_), ID).

node_aux(Node, Aux) :- arg(5, Node, Aux).

node_var_low_high(Node, Var, Low, High) :-
        Node = node(_,Var,Low,High,_).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   sat_bdd/2 converts a SAT formula in canonical form to an ordered
   and reduced zero-suppressed BDD.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

sat_bdd(V, Node)           :- var(V), !, var_projection(V, Node).
sat_bdd(0, 0)              :- !.
sat_bdd(1, Node)           :- !, taut(Node).
sat_bdd(card(Is,Fs), Node) :- !, counter_network(Is, Fs, Node).
sat_bdd(Sat, Node)         :- !,
        Sat =.. [F,A,B],
        sat_bdd(A, NA),
        sat_bdd(B, NB),
        apply(F, NA, NB, Node).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Counter network for card(Is,Fs).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

counter_network(Is, Vs, Node) :-
        maplist(exactly_k(Vs), Is, [N|Ns]),
        foldl(bdd_or, Ns, N, Node).

bdd_or(A, B, Node) :- apply(+, A, B, Node).

exactly_k(Vs, K, Node) :-
        must_be(integer, K),
        all_variables_in_index_order(Vars),
        exactly_k_(Vars, Vs, K, Node).

exactly_k_([], _, K, T) :-
        (   K =:= 0 -> T = 1
        ;   T = 0
        ).
exactly_k_([V|Vs], Fs, K0, Node) :-
        (   K0 =:= 0 ->
            vs_not_in_fs([V|Vs], Fs, Vars),
            taut_(Vars, Node)
        ;   member(Var, Fs), Var == V ->
            exactly_k_(Vs, Fs, K0, Left),
            K1 is K0 - 1,
            exactly_k_(Vs, Fs, K1, Right),
            make_node(V, Left, Right, Node)
        ;   exactly_k_(Vs, Fs, K0, Node0),
            make_node(V, Node0, Node0, Node)
        ).

vs_not_in_fs([], _, []).
vs_not_in_fs([V|Vs], Fs, Vars) :-
        (   member(F, Fs), F == V -> Vars = Rest
        ;   Vars = [V|Rest]
        ),
        vs_not_in_fs(Vs, Fs, Rest).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Compute F(NA, NB).

   We use a DCG to thread through an implicit argument G0, an
   association table F(IDA,IDB) -> Node, used for memoization.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

apply(F, NA, NB, Node) :-
        empty_assoc(G0),
        phrase(apply(F, NA, NB, Node), [G0], _).

apply(F, NA, NB, Node) -->
        { node_id(NA, IDA), node_id(NB, IDB), Key =.. [F,IDA,IDB] },
        (   state(G0), { get_assoc(Key, G0, Node) } -> []
        ;   apply_(F, NA, NB, Node),
            state(G0, G),
            { put_assoc(Key, G0, Node, G) }
        ).

apply_(*, F, G, Node) -->
        (   { F == 0 } -> { Node = 0 }
        ;   { G == 0 } -> { Node = 0 }
        ;   { F == G } -> { Node = F }
        ;   { var_less_than(F, G) } ->
            { node_var_low_high(F, _, LF, _) },
            apply(*, LF, G, Node)
        ;   { var_less_than(G, F) } ->
            { node_var_low_high(G, _, LG, _) },
            apply(*, F, LG, Node)
        ;   { node_var_low_high(F, Var, LF, HF),
              node_var_low_high(G, _, LG, HG) },
            apply(*, LF, LG, Low),
            apply(*, HF, HG, High),
            make_node(Var, Low, High, Node)
        ).


apply_(+, F, G, Node) -->
        (   { F == 0 } -> { Node = G }
        ;   { G == 0 } -> { Node = F }
        ;   { F == G } -> { Node = F }
        ;   { var_less_than(F, G) } ->
            { node_var_low_high(F, Var, LF, HF) },
            apply(+, LF, G, Low),
            make_node(Var, Low, HF, Node)
        ;   { var_less_than(G, F) } ->
            { node_var_low_high(G, Var, LG, HG) },
            apply(+, F, LG, Low),
            make_node(Var, Low, HG, Node)
        ;   { node_var_low_high(F, Var, LF, HF),
              node_var_low_high(G, _, LG, HG) },
            apply(+, LF, LG, Low),
            apply(+, HF, HG, High),
            make_node(Var, Low, High, Node)
        ).

apply_(#, F, G, Node) -->
        (   { F == 0 } -> { Node = G }
        ;   { G == 0 } -> { Node = F }
        ;   { F == G } -> { Node = 0 }
        ;   { var_less_than(F, G) } ->
            { node_var_low_high(F, Var, LF, HF) },
            apply(#, LF, G, Low),
            make_node(Var, Low, HF, Node)
        ;   { var_less_than(G, F) } ->
            { node_var_low_high(G, Var, LG, HG) },
            apply(#, F, LG, Low),
            make_node(Var, Low, HG, Node)
        ;   { node_var_low_high(F, Var, LF, HF),
              node_var_low_high(G, _, LG, HG) },
            apply(#, LF, LG, Low),
            apply(#, HF, HG, High),
            make_node(Var, Low, High, Node)
        ).


node_varindex(Node, VI) :-
        node_var_low_high(Node, V, _, _),
        var_index(V, VI).

var_less_than(NA, NB) :-
        (   integer(NB) -> true
        ;   node_varindex(NA, VAI),
            node_varindex(NB, VBI),
            VAI < VBI
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Access implicit state in DCGs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

state(S) --> state(S, S).

state(S0, S), [S] --> [S0].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Unification. X = Expr is equivalent to sat(X =:= Expr).

   Current limitation:
   ===================

   Unification of variables is not yet fully implemented.

   If you have no simultaneous unifications, you can comment out the
   exception instantiation_not_yet_supported and hence use labeling/1.

   Aliasing is definitely not supported.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

verify_attributes(Var, Other, Gs) :-
        % can the invocations of verify_attributes/3 be reduced?
        %portray_clause(verify(Var=Other)),
        (   get_attr(Var, clpb, index_root(I,Root)) ->
            %portray_clause(verify(index_root(I,Root))),
            (   integer(Other) ->
                throw(instantiation_not_supported),
                (   between(0, 1, Other) ->
                    restrict_in_all_bdds(I, Root, Other)
                ;   no_truth_value(Other)
                )
            ;   throw(aliasing_not_supported),
                parse_sat(Other, OtherSat),
                root_get_formula_bdd(Root, Sat0, _),
                Sat = Sat0*OtherSat,
                sat_roots(Sat, Roots),
                maplist(root_rebuild_bdd, Roots),
                taut(True),
                roots_and(Roots, 1-True, And-BDD1),
                maplist(del_bdd, Roots),
                maplist(=(NewRoot), Roots),
                root_put_formula_bdd(NewRoot, And, BDD1),
                is_bdd(BDD1),
                satisfiable_bdd(BDD1)
            )
        ;   true
        ),
        Gs = [].

restrict_in_all_bdds(VI, Root, Other) :-
        all_variables_in_index_order(Vs),
        maplist(var_index_root, Vs, _, Roots0),
        term_variables([Root|Roots0], Roots),
        maplist(restrict_vi(VI,Other), Roots, BDDs),
        maplist(satisfiable_bdd, BDDs).

restrict_vi(VI, Other, Root, BDD) :-
        root_get_formula_bdd(Root, Sat, BDD0),
        bdd_restriction(BDD0, VI, Other, BDD),
        root_put_formula_bdd(Root, Sat, BDD).

root_rebuild_bdd(Root) :-
        (   root_get_formula_bdd(Root, F0, _) ->
            parse_sat(F0, Sat),
            sat_bdd(Sat, BDD),
            root_put_formula_bdd(Root, F0, BDD)
        ;   true
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BDD restriction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bdd_restriction(Node, VI, Value, Res) :-
        empty_assoc(G0),
        phrase(bdd_restriction_(Node, VI, Value, Res), [G0], _),
        is_bdd(Res).

bdd_restriction_(Node, VI, Value, Res) -->
        (   { integer(Node) } -> { Res = Node }
        ;   { node_var_low_high(Node, Var, Low, High) } ->
            (   { integer(Var) } ->
                (   { Var =:= 0 } -> bdd_restriction_(Low, VI, Value, Res)
                ;   { Var =:= 1 } -> bdd_restriction_(High, VI, Value, Res)
                ;   { no_truth_value(Var) }
                )
            ;   { var_index(Var, I0),
                  node_id(Node, ID) },
                (   { I0 =:= VI } ->
                    (   { Value =:= 0 } -> { Res = Low }
                    ;   { Value =:= 1 } -> { Res = High }
                    ;   { throw(cannot_happen) }
                    )
                ;   { I0 > VI } -> { Res = Node }
                ;   state(G0), { get_assoc(ID, G0, Res) } -> []
                ;   bdd_restriction_(Low, VI, Value, LRes),
                    bdd_restriction_(High, VI, Value, HRes),
                    make_node(Var, LRes, HRes, Res),
                    state(G0, G),
                    { put_assoc(ID, G0, Res, G) }
                )
            )
        ;   { domain_error(node, Node) }
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Relating a BDD to its elements (nodes and variables).

   Note that BDDs can become quite big (easily more than a million
   nodes), and memory space is a major bottleneck for many problems. If
   possible, we therefore do not duplicate the entire BDD in memory (as
   in bdd_ites/2), but only extract its features as needed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bdd_nodes(BDD, Ns) :- bdd_nodes(do_nothing, BDD, Ns).

do_nothing(_).

% VPred is a unary predicate that is called for each branching variable

bdd_nodes(VPred, BDD, Ns) :-
        phrase(bdd_nodes_(VPred, BDD), Ns),
        maplist(with_aux(unvisit), Ns).

bdd_nodes_(VPred, Node) -->
        (   { integer(Node) ;  with_aux(is_visited, Node) } -> []
        ;   { with_aux(put_visited, Node),
              node_var_low_high(Node, Var, Low, High),
              call(VPred, Var) },
            [Node],
            bdd_nodes_(VPred, Low),
            bdd_nodes_(VPred, High)
        ).

bdd_variables(BDD, Vs) :-
        bdd_nodes(BDD, Nodes),
        nodes_variables(Nodes, Vs).

nodes_variables(Nodes, Vs) :-
        phrase(nodes_variables_(Nodes), Vs),
        maplist(unvisit, Vs).

nodes_variables_([]) --> [].
nodes_variables_([Node|Nodes]) -->
        { node_var_low_high(Node, Var, _, _) },
        (   { integer(Var) } -> []
        ;   (   { is_visited(Var) } -> []
            ;   { put_visited(Var) },
                [Var]
            )
        ),
        nodes_variables_(Nodes).

unvisit(V) :- del_attr(V, clpb_visited).

is_visited(V) :- get_attr(V, clpb_visited, true).

put_visited(V) :- put_attr(V, clpb_visited, true).

with_aux(Pred, Node) :-
        node_aux(Node, Aux),
        call(Pred, Aux).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Internal consistency checks.

   To enable these checks, define the fact clpb_validation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic(clpb_validation/0).

%clpb_validation.

is_bdd(BDD) :-
        (   clpb_validation ->
            bdd_ites(BDD, ITEs),
            % length(ITEs, Len),
            % portray_clause(Len),
            pairs_values(ITEs, Ls0),
            sort(Ls0, Ls1),
            (   same_length(Ls0, Ls1) -> true
            ;   domain_error(reduced_ites, (ITEs,Ls0,Ls1))
            )
            % (   member(ITE, ITEs), \+ registered_node(ITE) ->
            %     domain_error(registered_node, ITE)
            % ;   true
            % )
        ;   true
        ).

registered_node(Node-ite(Var,High,Low)) :-
        low_high_key(Low, High, Key),
        lookup_node(Var, Key, Node0),
        Node == Node0.

bdd_ites(BDD, ITEs) :-
        bdd_nodes(BDD, Nodes),
        maplist(node_ite, Nodes, ITEs).

node_ite(Node, Node-ite(Var,High,Low)) :-
        node_var_low_high(Node, Var, Low, High).

%% labeling(+Vs) is nondet.
%
% Assigns truth values to the Boolean variables Vs such that all
% stated constraints are satisfied.

labeling(Vs0) :-
        must_be(list, Vs0),
        variables_in_index_order(Vs0, Vs),
        maplist(indomain, Vs).

variables_in_index_order(Vs0, Vs) :-
        maplist(var_with_index, Vs0, IVs0),
        keysort(IVs0, IVs),
        pairs_values(IVs, Vs).

var_with_index(V, I-V) :-
        (   var_index_root(V, I, _) -> true
        ;   I = 0
        ).

indomain(0).
indomain(1).


%% sat_count(+Expr, -N) is det.
%
% N is the number of different assignments of truth values to CLP(B)
% variables, such that the Boolean expression Expr is true and all
% posted constraints are satisfiable.
%
% Example:
%
% ==
% ?- length(Vs, 120), zdd_set_vars(Vs),  sat_count(+Vs, N).
% N = 1329227995784915872903807060280344575.
% ==

sat_count(Sat0, N) :-
        catch((parse_sat(Sat0, Sat),
               sat_bdd(Sat, BDD),
               sat_roots(Sat, Roots),
               roots_and(Roots, _-BDD, _-BDD1),
               bdd_count(BDD1, N),
               % reset all attributes and Aux variables
               throw(count(N))),
              count(N),
              true).

sat_count(N) :-
        catch((all_variables_in_index_order([First|_]),
               var_index_root(First, _, Root),
               root_get_formula_bdd(Root, _, BDD),
               bdd_count(BDD, N),
               throw(count(N))),
              count(N),
              true).

bdd_count(Node, Count) :-
        (   integer(Node) -> Count = Node
        ;   node_aux(Node, Count),
            (   integer(Count) -> true
            ;   node_var_low_high(Node, _, Low, High),
                bdd_count(Low, LCount),
                bdd_count(High, HCount),
                Count is LCount + HCount
            )
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   You must enable unification if you want to generate random solutions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

random_solution(Seed, Vars) :-
        must_be(list, Vars),
        set_random(seed(Seed)),
        (   ground(Vars) -> true
        ;   catch((sat(+[1|Vars]), % capture all variables with a single BDD
                   once((member(Var, Vars),var(Var))),
                   var_index_root(Var, _, Root),
                   root_get_formula_bdd(Root, _, BDD),
                   phrase(random_bindings(BDD), Bs),
                   maplist(del_attrs, Vars),
                   % reset all attribute modifications
                   throw(randsol(Vars, Bs))),
                  randsol(Vars, Bs),
                  true),
            maplist(call, Bs),
            % set remaining variables to 0
            include(var, Vars, Remaining),
            maplist(=(0), Remaining)
        ).

random_bindings(Node) --> { Node == 1 }, !.
random_bindings(Node) -->
        { node_var_low_high(Node, Var, Low, High),
          bdd_count(Node, Total),
          bdd_count(Low, LCount) },
        (   { maybe(LCount, Total) } ->
            [Var=0], random_bindings(Low)
        ;   [Var=1], random_bindings(High)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Projection to residual goals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attribute_goals(Var) -->
        { var_index_root(Var, _, Root) },
        boolean(Var),
        (   { root_get_formula_bdd(Root, _, BDD) } ->
%            [bdd=BDD],
            { del_bdd(Root),
              bdd_variables(BDD, Vs),
              maplist(del_clpb, Vs),
              bdd_nodes(BDD, Nodes) },
            nodes(Nodes)
        ;   []
        ).

boolean(V) --> [sat(V =:= V)].

nodes([]) --> [].
nodes([Node|Nodes]) -->
        { node_var_low_high(Node, Var, Low, High),
          maplist(node_projection, [Node,High,Low], [ID,HID,LID]) },
        [ID-(Var -> HID ; LID)],
        nodes(Nodes).

node_projection(Node, Projection) :-
        node_id(Node, ID),
        (   integer(ID) -> Projection = node(ID)
        ;   Projection = ID
        ).

del_clpb(Var) :-
        %del_attr(Var, clpb),
        del_attr(Var, clpb_hash).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Global variables for unique node and variable IDs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_clpb_var('$clpb_next_var') :- bb_put('$clpb_next_var', 0).

make_clpb_var('$clpb_next_node') :- bb_put('$clpb_next_node', 0).

:- initialization((make_clpb_var(_),false;true)).

clpb_next_id(Var, ID) :-
        bb_get(Var, ID),
        Next is ID + 1,
        bb_b_put(Var, Next).

:- use_module(library(format)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- clpb:zdd_set_vars([X,Y]).

?- clpb:zdd_set_vars([X,Y,Z]), sat(card([2],[X,Y,Z])).

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

zdd_set_vars(Vars) :-
        must_be(list(var), Vars),
        bb_b_put('$clpb_vars', Vars),
        maplist(enumerate_variable, Vars),
        sat(+[1|Vars]).

taut(Node) :-
        all_variables_in_index_order(Vars),
        taut_(Vars, Node).

taut_([], 1).
taut_([V|Vs], Taut) :-
        taut_(Vs, Taut0),
        make_node(V, Taut0, Taut0, Taut).

var_projection(V, Node) :-
        all_variables_in_index_order(Vars),
        projection_(Vars, V, Node).

projection_([Var|Vars], V, Proj) :-
        (   Var == V ->
            taut_(Vars, Taut),
            make_node(V, 0, Taut, Proj)
        ;   projection_(Vars, V, Proj0),
            make_node(Var, Proj0, Proj0, Proj)
        ).

all_variables_in_index_order(Vars) :-
        bb_get('$clpb_vars', Vars0),
        include(var, Vars0, Vars).
