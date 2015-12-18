:- use_module(library(clpb)).
:- use_module(library(clpfd)).

%?- edge(X, Y).


cycle(100).

%?- edge(X, Y), writeln(X-Y), false.


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
   %@ Ms = [-1, -1, 1, -1, 1, 1, -1, -1, 1|...].

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

%?- time(maximum_thue_morse_kernel(Is, Negatives, Max)).
%@ % 32,886,119 inferences, 6.049 CPU in 6.068 seconds (100% CPU, 5436963 Lips)
%@ Is = [1, 3, 6, 9, 12, 15, 18, 20, 23|...],
%@ Negatives = [1, 25, 41, 73, 97],
%@ Max = 28 .

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
%@ Sat = ~ (... + ... + ... * ... + _G117*_G118+_G118*_G117+_G118*_G119+_G119*_G118+_G20*_G119+_G119*_G20)* (... * ... * (... + ...)* (_G113+ (... + ...))* (_G114+ (... + ... + _G115))* (_G115+ (0+_G114+_G116))* (_G116+ (0+_G115+_G117))* (_G117+ (0+_G116+_G118))* (_G118+ (0+_G117+_G119))* (_G119+ (0+_G20+_G118))),
%@ C = 1630580875002.

%?- independent(I, Nodes, Vars, _), sat(I), labeling(Vars), writeln(Vars), false.

run :-
        kernel(_, Vs, K),
        sat(K),
        labeling(Vs),
        writeln(Vs),
        false.

%?- time(run).
%@ [0,0,1,0,0,1]
%@ [0,1,0,0,1,0]
%@ [0,1,0,1,0,1]
%@ [1,0,0,1,0,0]
%@ [1,0,1,0,1,0]
%@ % 47,903 inferences, 0.011 CPU in 0.012 seconds (97% CPU, 4236579 Lips)
%@ false.

%@ [0,0,1,0,0,1]
%@ [0,1,0,0,1,0]
%@ [0,1,0,1,0,1]
%@ % 42,088 inferences, 0.009 CPU in 0.010 seconds (95% CPU, 4611373 Lips)
%@ false.
