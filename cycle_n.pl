:- use_module(clpb).

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
        N2 is N1 + 1,
        cycle(Limit),
        N2 =< Limit,
        edge_(N1, N2, X, Y).




/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   IND(X) = not OR_(u->v){ x_u /\ x_v }
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

independent(~ Or, Nodes, Vars, Assoc) :-
        findall(A-B, edge(A, B), Edges),
        pairs_keys_values(Edges, Ks, Vs),
        append(Ks, Vs, Ns0),
        sort(Ns0, Nodes),
        pairs_keys_values(Pairs, Nodes, Vars),
        list_to_assoc(Pairs, Assoc),
        maplist(edge_and(Assoc), Edges, Ands),
        foldl(or, Ands, 0, Or).

or(B, A, A+B).


edge_and(Assoc, A0-B0, A*B) :-
        get_assoc(A0, Assoc, A),
        get_assoc(B0, Assoc, B).

kernel(Nodes, Vars, Ind*And) :-
        independent(Ind, Nodes, Vars, Assoc),
        maplist(node_or(Assoc), Nodes, Ors),
        foldl(and, Ors, 1, And).

and(X, Y, Y * X).

node_or(Assoc, Node, Var + Or) :-
        get_assoc(Node, Assoc, Var),
        findall(U-Node, edge(U, Node), Edges0),
        sort(Edges0, Edges),
        pairs_keys(Edges, Us),
        maplist(u_to_var(Assoc), Us, Vars),
        foldl(or, Vars, 0, Or).

u_to_var(Assoc, Node, Var) :- get_assoc(Node, Assoc, Var).

%?- kernel(_,_,Sat), clpb:sat_count(Sat, C).
%@ Sat = ~ (... + ... + ... * ... + _G117*_G118+_G118*_G117+_G118*_G119+_G119*_G118+_G20*_G119+_G119*_G20)* (... * ... * (... + ...)* (_G113+ (... + ...))* (_G114+ (... + ... + _G115))* (_G115+ (0+_G114+_G116))* (_G116+ (0+_G115+_G117))* (_G117+ (0+_G116+_G118))* (_G118+ (0+_G117+_G119))* (_G119+ (0+_G20+_G118))),
%@ C = 1630580875002.

%?- independent(I, Nodes, Vars), sat(I), labeling(Vars), writeln(Vars), false.

%?- kernel(Ns, Vs), labeling(Vs), writeln(Vs), false.

run :-
        kernel(_, Vs, _),
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