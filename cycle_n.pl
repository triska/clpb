:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(format)).
:- use_module(library(time)).
:- use_module(library(assoc)).
:- use_module(library(between)).

%?- edge(X, Y).


cycle(100).

%?- edge(X, Y), portray_clause(X-Y), false.


edge(X, Y) :- edge_(1, 2, X, Y).

edge(X, Y) :-
        cycle(Limit),
        (   X = 1, Y = Limit
        ;   X = Limit, Y = 1
        ).

edge_(A, B, A, B).
edge_(B, A, A, B).
edge_(_, N1, X, Y) :-
        N2 #= N1 + 1,
        cycle(Limit),
        N2 #=< Limit,
        edge_(N1, N2, X, Y).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Node N has weight w_N = (-1)^n(N), where n(N) is the number of 1s in
  the binary representation of N. This is similar to Thue-Morse codes.

  For example, the Thue-Morse weights of the integers 1,...,10 are:

   ?- thue_morse_weights(10, Ms).
   %@    Ms = [-1,-1,1,-1,1,1,-1,-1,1,1].

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

integer_binaries(I, Bs) :-
        once(phrase(binaries(I, 0), Bs0)),
        reverse(Bs0, Bs).

binaries(0, _) --> [].
binaries(I, E0) -->
        (   { I mod 2 #= 0 } -> [0]
        ;   [1]
        ),
        { I1 #= I // 2, E1 #= E0 + 1 },
        binaries(I1, E1).

thue_morse_weights(N, Ms) :-
        length(Ms, N),
        numlist(1, N, Ns),
        maplist(integer_binaries, Ns, Bss),
        maplist(sum_list, Bss, Cards),
        maplist(neg1_pow, Cards, Ms).

neg1_pow(Card, Pow) :- Pow #= (-1)^Card.

maximum_thue_morse_kernel(Is, Negatives, Max) :-
        kernel(_, Vs, K),
        sat(K),
        length(Vs, L),
        thue_morse_weights(L, Weights),
        weighted_maximum(Weights, Vs, Max),
        numlist(1, L, Ns),
        pairs_keys_values(Pairs0, Vs, Ns),
        include(key_one, Pairs0, Pairs),
        pairs_values(Pairs, Is),
        pairs_keys_values(WNs, Weights, Ns),
        pairs_keys_values(WPairs0, Vs, WNs),
        include(key_one, WPairs0, WPairs1),
        pairs_values(WPairs1, WPairs2),
        include(key_negative, WPairs2, WPairs),
        pairs_values(WPairs, Negatives).

key_negative(K-_) :- K #< 0.

key_one(1-_).

include(_, [], []).
include(G_1, [L|Ls0], Ls) :-
        (   call(G_1, L) ->
            Ls = [L|Rs]
        ;   Ls = Rs
        ),
        include(G_1, Ls0, Rs).

%?- time(maximum_thue_morse_kernel(Is, Negatives, Max)).
%@    % CPU time: 125.236s
%@    Is = [1,3,6,9,12,15,18,20,23,25,27,30,33,36,39,41,43,46,48,51,...], Negatives = [1,25,41,73,97], Max = 28
%@ ;  ... .

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   IND(X) = not OR_(u->v){ x_u /\ x_v }
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

independent(~ +(Ands), Nodes, Vars, Assoc) :-
        findall(A-B, edge(A, B), Edges),
        pairs_keys_values(Edges, Ks, Vs),
        append(Ks, Vs, Ns0),
        sort(Ns0, Nodes),
        pairs_keys_values(Pairs, Nodes, Vars),
        list_to_assoc(Pairs, Assoc),
        maplist(edge_and(Assoc), Edges, Ands).


edge_and(Assoc, A0-B0, A*B) :-
        get_assoc(A0, Assoc, A),
        get_assoc(B0, Assoc, B).

kernel(Nodes, Vars, Ind* *(Ors)) :-
        independent(Ind, Nodes, Vars, Assoc),
        maplist(node_or(Assoc), Nodes, Ors).

node_or(Assoc, Node, Var + +(Vars)) :-
        get_assoc(Node, Assoc, Var),
        findall(U-Node, edge(U, Node), Edges0),
        sort(Edges0, Edges),
        pairs_keys(Edges, Us),
        maplist(u_to_var(Assoc), Us, Vars).

u_to_var(Assoc, Node, Var) :- get_assoc(Node, Assoc, Var).

%?- kernel(_,_,Sat), sat_count(Sat, C).
%@    Sat = ~ +[_A*_B,_B*_A,_B*_C,_C*_B,_C*_D,_D*_C,_D*_E,_E*_D,_E*_F,_F*_E,_F*_G,_G*_F,_G*_H,_H*_G,_H*_I,_I*_H,... * ...,...]* *([_A+ +[_B,_J],_B+ +[_A,_C],_C+ +[_B,_D],_D+ +[_C,_E],_E+ +[_D,_F],_F+ +[_E,_G],_G+ +[_F,_H],_H+ +[_G,_I],_I+ +[_H,_K],_K+ +[_I,_L],_L+ +[_K,_M],_M+ +[_L,_N],_N+ +[_M,_O],_O+ +[_N,_P],_P+ +[_O,...],_Q+ +[...],_R+ + ...,... + ...,...]), C = 1630580875002
%@ ;  false.

%?- independent(I, Nodes, Vars, _), sat(I), labeling(Vars), portray_clause(Vars), false.

run :-
        kernel(_, Vs, K),
        sat(K),
        labeling(Vs),
        portray_clause(Vs),
        false.

%?- time(run).
%@ [0,0,1,0,0,1].
%@ [0,1,0,0,1,0].
%@ [0,1,0,1,0,1].
%@ [1,0,0,1,0,0].
%@ [1,0,1,0,1,0].
%@    % CPU time: 0.290s
%@    false.
