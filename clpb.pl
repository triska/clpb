/*  CLP(B) based on binary decision diagrams.
    Preliminary version.

    Author:        Markus Triska
    E-mail:        triska@gmx.at
    Written:       August 2014

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

%?- sat((A + B + B) + (A + B + B)*D*E).

%?- taut(X + ~X, T).

%?- sat(X+(Y* ~Y)).

%?- sat(X * Y), labeling([X,Y]).

%?- sat(X*Y).
%@ node(7): (v_i(X, 2)->node(6);false),
%@ node(6): (v_i(Y, 3)->true;false).

%?- sat(A* ~A +B).

%?- sat(C* ~C + B* ~B + D).

%?- sat(~ (A+B)), labeling([A,B]).

%?- sat(~ (A+B)), B = 0.

%?- sat(A*A + C).

%?- sat(X), X = Y.

%?- sat(X+Y), sat(A+B), A = X.

%?- sat(X), sat(Y), X = Y, X = 0.

%?- sat(X=:=Y).
%@ node(4): (v_i(X, 0)->node(2);node(3)),
%@ node(3): (v_i(Y, 1)->false;true),
%@ node(2): (v_i(Y, 1)->true;false).

%?- sat(A+B), A = 1.

%?- sat(A =:= B), A = 1.

%?- sat(A* ~A + B).
%?- sat(A* ~A).

%?- sat(A+B).

%?- sat(A* ~A + B).

%?- sat(A+A), A = 1.



:- module(clpb, [
                 op(300, fy, ~),
                 op(500, yfx, #),
                 sat/1,
                 taut/2,
                 labeling/1
                ]).

:- use_module(library(error)).
:- use_module(library(assoc)).
:- use_module(library(apply_macros)).

                                %bool_get(X, BDDs) :- get_attr(X, clpb, BDDs).

state(S) --> state(S, S).

state(S0, S), [S] --> [S0].


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

% wrap variables with v(...) and integers with i(...)
sat_nondefaulty(V, v(V)) :- var(V), !.
sat_nondefaulty(I, i(I)) :- integer(I), !.
sat_nondefaulty(~A0, ~A) :- !, sat_nondefaulty(A0, A).
sat_nondefaulty(S0, S) :-
        S0 =.. [F,X0,Y0],
        sat_nondefaulty(X0, X),
        sat_nondefaulty(Y0, Y),
        S =.. [F,X,Y].

% elementary
sat_rewrite(v(V), v(V)).
sat_rewrite(i(I), i(I)).
sat_rewrite(P0*Q0, P*Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(P0+Q0, P+Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(P0#Q0, P#Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
%sat_rewrite(X^P0, X^P)  :- sat_rewrite(P0, P).
% synonyms
sat_rewrite(~P, R)      :- sat_rewrite(i(1) # P, R).
sat_rewrite(P =:= Q, R) :- sat_rewrite(~P # Q, R).
sat_rewrite(P =\= Q, R) :- sat_rewrite(P # Q, R).
sat_rewrite(P =< Q, R)  :- sat_rewrite(~P + Q, R).
sat_rewrite(P >= Q, R)  :- sat_rewrite(Q =< P, R).
sat_rewrite(P < Q, R)   :- sat_rewrite(~P * Q, R).
sat_rewrite(P > Q, R)   :- sat_rewrite(Q < P, R).

%?- clpb:sat_rewrite(~v(P)> v(Q), R).

                                %?- gtrace, clpb:sat(A+B).

                                %?- clpb:sat(A+A+B).

must_be_sat(Sat) :-
        (   is_sat(Sat) -> true
        ;   domain_error(boolexpr, Sat)
        ).

parse_sat(Sat0, Sat) :-
        must_be_sat(Sat0),
        sat_nondefaulty(Sat0, Sat1),
        sat_rewrite(Sat1, Sat),
        term_variables(Sat, Vs),
        maplist(enumerate_variable, Vs).

sat_roots(Sat, Roots) :-
        term_variables(Sat, Vs),
        maplist(var_index_root, Vs, _, Roots0),
        term_variables(Roots0, Roots).

sat(Sat0) :-
        parse_sat(Sat0, Sat),
        sat_bdd(Sat, BDD),
        sat_roots(Sat, Roots),
        foldl(root_and, Roots, BDD, BDD1),
        maplist(del_bdd, Roots),
        maplist(=(Root), Roots),
        put_attr(Root, bdd, BDD1),
        satisfiable_bdd(BDD1).

del_bdd(Root) :- del_attr(Root, bdd).

root_and(Root, BDD0, BDD) :-
        (   get_attr(Root, bdd, B) ->
            bdd_and(B, BDD0, BDD)
        ;   BDD = BDD0
        ).


taut(Sat0, Truth) :-
        parse_sat(Sat0, Sat),
        sat_roots(Sat, Roots),
        foldl(root_and, Roots, 1, Ands),
        (   sat_bdd(Sat, BDD), bdd_and(BDD, Ands, B), B == 0 ->
            Truth = 0
        ;   sat_bdd(i(1)#Sat, BDD), bdd_and(BDD, Ands, B), B == 0 ->
            Truth = 1
        ;   false
        ).

satisfiable_bdd(BDD) :-
        (   BDD == 0 -> false
        ;   BDD == 1 -> true
        ;   node_var_low_high(BDD, Var, Low, High),
            (   Low == 0 -> Var = 1
            ;   High == 0 -> Var = 0
            ;   true
            )
        ).

var_index(V, I) :- var_index_root(V, I, _).

var_index_root(V, I, Root) :- get_attr(V, clpb, var_index_root(_,I,Root)).

enumerate_variable(V) :-
        (   var_index_root(V, _, _) -> true
        ;   nb_getval('$clpb_next_var', Index0),
            put_attr(V, clpb, var_index_root(V,Index0,_)),
            Index is Index0 + 1,
            nb_setval('$clpb_next_var', Index)
        ).


%?- sat(X+Y).

bdd_and(NA, NB, And) :-
        empty_assoc(H0),
        empty_assoc(G0),
        phrase(apply(*, NA, NB, And), [H0-G0], _).


bool_op(+, 0, 0, 0).
bool_op(+, 0, 1, 1).
bool_op(+, 1, 0, 1).
bool_op(+, 1, 1, 1).

bool_op(*, 0, 0, 0).
bool_op(*, 0, 1, 0).
bool_op(*, 1, 0, 0).
bool_op(*, 1, 1, 1).

bool_op(#, 0, 0, 0).
bool_op(#, 0, 1, 1).
bool_op(#, 1, 0, 1).
bool_op(#, 1, 1, 0).

make_node(Var, Low, High, Node) -->
        state(H0-G0, H-G0),
        { (   Low == High -> Node = Low, H0 = H
          ;   node_id(Low, LID),
              node_id(High, HID),
              var_index(Var, VI),
              Triple = node(VI,LID,HID),
              (   get_assoc(Triple, H0, Node) -> H0 = H
              ;   put_attr(Node, triple, node(Var,Low,High)),
                  nb_getval('$clpb_next_node', ID0),
                  put_attr(Node, id, ID0),
                  ID is ID0 + 1,
                  nb_setval('$clpb_next_node', ID),
                  put_assoc(Triple, H0, Node, H)
              )
          ) }.


sat_bdd(Sat, BDD) :-
        empty_assoc(H0),
        empty_assoc(G0),
        phrase(sat_bdd(Sat, BDD), [H0-G0], _).

sat_bdd(i(I), I) --> !.
sat_bdd(v(V), Node) --> !, make_node(V, 0, 1, Node).
sat_bdd(Sat, Node) -->
        { Sat =.. [F,A,B] },
        sat_bdd(A, NA),
        sat_bdd(B, NB),
        apply(F, NA, NB, Node).

node_id(Node, ID) :-
        (   integer(Node) ->
            (   Node =:= 0 -> ID = false
            ;   Node =:= 1 -> ID = true
            ;   domain_error(boolean, Node)
            )
        ;   get_attr(Node, id, ID0),
            ID = node(ID0)
        ).

node_var_low_high(Node, Var, Low, High) :-
        get_attr(Node, triple, node(Var,Low,High)).

node_varindex(Node, VI) :-
        node_var_low_high(Node, V, _, _),
        var_index(V, VI).

var_less_than(NA, NB) :-
        (   integer(NB) -> true
        ;   node_varindex(NA, VAI),
            node_varindex(NB, VBI),
            VAI < VBI
        ).

apply(F, NA, NB, Node) -->
        (   { integer(NA), integer(NB) } -> { once(bool_op(F, NA, NB, Node)) }
        ;   { node_id(NA, IDA), node_id(NB, IDB) },
            (   state(_-G0), { get_assoc(g(F,IDA,IDB), G0, Node) } -> []
            ;   apply_(F, NA, NB, Node),
                state(H0-G0, H0-G),
                { put_assoc(g(F,IDA,IDB), G0, Node, G) }
            )
        ).

apply_(F, NA, NB, Node) -->
        { var_less_than(NA, NB),
          !,
          node_var_low_high(NA, VA, LA, HA) },
        apply(F, LA, NB, Low),
        apply(F, HA, NB, High),
        make_node(VA, Low, High, Node).
apply_(F, NA, NB, Node) -->
        { node_var_low_high(NA, VA, LA, HA),
          node_var_low_high(NB, VB, LB, HB),
          VA == VB },
        !,
        apply(F, LA, LB, Low),
        apply(F, HA, HB, High),
        make_node(VA, Low, High, Node).
apply_(F, NA, NB, Node) --> % NB > NA
        { node_var_low_high(NB, VB, LB, HB) },
        apply(F, NA, LB, Low),
        apply(F, NA, HB, High),
        make_node(VB, Low, High, Node).


attr_unify_hook(var_index_root(_,I,Root), Other) :-
        (   integer(Other) ->
            (   between(0, 1, Other) ->
                get_attr(Root, bdd, BDD0),
                bdd_restriction(BDD0, I, BDD),
                put_attr(Root, bdd, BDD),
                satisfiable_bdd(BDD)
            ;   domain_error(boolean, Other)
            )
        ;   representation_error('please use sat(X=:=Y) instead of X = Y')
        ).

%?- sat((~X)*X + B).

is_bdd(BDD) :-
        catch((phrase(bdd_ite(BDD), ITEs0),
               maplist(ite_ground, ITEs0, Ls0),
               sort(Ls0, Ls1),
               (   same_length(Ls0, Ls1) -> throw(is_ok)
               ;   domain_error(reduced_ites, (ITEs0,Ls0,Ls1))
               )),
              is_ok,
              true).

ite_ground(_:(v_i(_,I) -> HID ; LID), t(I,HID,LID)).



bdd_restriction(Node, VI, Res) :-
        empty_assoc(H0),
        empty_assoc(G0),
        phrase(bdd_restriction_(Node, VI, Res), [H0-G0], _).
        %is_bdd(Res),

bdd_restriction_(Node, VI, Res) -->
        (   { integer(Node) } -> { Res = Node }
        ;   { node_var_low_high(Node, Var, Low, High) } ->
            (   { integer(Var) } ->
                (   { Var =:= 0 } -> bdd_restriction_(Low, VI, Res)
                ;   { Var =:= 1 } -> bdd_restriction_(High, VI, Res)
                ;   { domain_error(boolean, Var) }
                )
            ;   (   { var_index(Var, I0),
                      node_id(Node, ID) },
                    (   { I0 > VI } -> { Res = Node }
                    ;   state(_-G0), { get_assoc(ID, G0, Res) } -> []
                    ;   bdd_restriction_(Low, VI, LRes),
                        bdd_restriction_(High, VI, HRes),
                        make_node(Var, LRes, HRes, Res),
                        state(H0-G0, H0-G),
                        { put_assoc(ID, G0, Res, G) }
                    )
                )
            )
        ;   { domain_error(node, Node) }
        ).

%?- sat(X+Y).

attribute_goals(Var) -->
        { var_index_root(Var, _, Root) },
        (   { get_attr(Root, bdd, BDD) } ->
            bdd_ite(BDD),
            { del_attr(Root, bdd) }
        ;   []
        ).

bdd_ite(B) -->
        bdd_ite_(B),
        { bdd_clear(B) }.

bdd_ite_(Node) -->
        (   { integer(Node) ;  get_attr(Node, visited, true) } -> []
        ;   { node_id(Node, ID) } ->
            { node_var_low_high(Node, Var, Low, High),
              var_index(Var, Index),
              put_attr(Node, visited, true),
              node_id(High, HID),
              node_id(Low, LID) },
            [ID : (v_i(Var,Index) -> HID ; LID )],
            bdd_ite_(Low),
            bdd_ite_(High)
        ;   []
        ).

bdd_clear(Node) :-
        (   node_var_low_high(Node, _, Low, High) ->
            bdd_clear(Low),
            bdd_clear(High),
            del_attrs(Node)
        ;   true
        ).

%?- sat(X+Y).

labeling(Xs) :-
        must_be(list, Xs),
        maplist(indomain, Xs).

indomain(0).
indomain(1).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   SATCount
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- clpb:sat_count(X, N).

sat_count(Sat0, N) :-
        nb_getval('$clpb_next_var', NextVar),
        nb_setval('$clpb_next_var', 1),
        catch((term_variables(Sat0, Vs),
               maplist(del_attrs, Vs),
               parse_sat(Sat0, Sat),
               sat_bdd(Sat, BDD),
               nb_getval('$clpb_next_var', VNum),
               bdd_count(BDD, VNum, Count),
               throw(count(Count))),
              count(N),
              true),
        nb_setval('$clpb_next_var', NextVar).

bdd_count(Node, VNum, Count) :-
        (   integer(Node) -> Count = Node
        ;   get_attr(Node, count, Count) -> true
        ;   node_var_low_high(Node, V, Low, High),
            bdd_count(Low, VNum, LCount),
            bdd_count(High, VNum, HCount),
            bdd_pow(Low, V, VNum, LPow),
            bdd_pow(High, V, VNum, HPow),
            Count is LPow*LCount + HPow*HCount,
            put_attr(Node, count, Count)
        ).

bdd_pow(Node, V, VNum, Pow) :-
        var_index(V, Index),
        (   integer(Node) -> P = VNum
        ;   node_varindex(Node, P)
        ),
        Pow is 2^(P - Index - 1).

make_clpb_var('$clpb_next_var') :- nb_setval('$clpb_next_var', 0).

make_clpb_var('$clpb_next_node') :- nb_setval('$clpb_next_node', 0).

:- multifile user:exception/3.

user:exception(undefined_global_variable, Name, retry) :-
        make_clpb_var(Name), !.

%?- sat(~X+Y).

%?- sat(X*Y).
%?- trace, sat(X).
%?- sat(X+Y), X = 1.
