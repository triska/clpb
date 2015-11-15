/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Triomino tiling of an N x N chessboard.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpb)).
:- use_module(library(clpfd)).

tile([[1,1],
      [1,0]]).

tile([[1,0],
      [1,1]]).

tile([[0,1],
      [1,1]]).

tile([[1,1],
      [0,1]]).

tile([[1,1,1]]).

tile([[1],
      [1],
      [1]]).

%?- triominoes(4, Vs, _, Sat), sat(Sat).
%@ false.

triominoes(N, Vs, Cs, *(Cs)) :-
        matrix(N, N, Rows),
        same_length(Rows, Vs),
        transpose(Rows, Cols),
        phrase(all_cardinalities(Cols, Vs), Cs).

all_cardinalities([], _) --> [].
all_cardinalities([Col|Cols], Vs) -->
        { pairs_keys_values(Pairs0, Col, Vs),
          include(key_one, Pairs0, Pairs),
          pairs_values(Pairs, Cs) },
        [card([1], Cs)],
        all_cardinalities(Cols, Vs).

key_one(1-_).


matrix(M, N, Ms) :-
        Squares #= M*N,
        length(Ls, Squares),
        findall(Ls, line(N,Ls), Ms0),
        sort(Ms0, Ms).

line(N, Ls) :-
        tile(Ts),
        length(Ls, Max),
        phrase((zeros(0,P0),tile_(Ts,N,Max,P0,P1),zeros(P1,_)), Ls).

tile_([], _, _, P, P) --> [].
tile_([T|Ts], N, Max, P0, P) -->
        tile_part(T, N, P0, P1),
        { (P1 - 1) mod N #>= P0 mod N,
          P2 #= min(P0 + N, Max) },
        zeros(P1, P2),
        tile_(Ts, N, Max, P2, P).

tile_part([], _, P, P) --> [].
tile_part([L|Ls], N, P0, P) -->
        [L],
        { P1 #= P0 + 1 },
        tile_part(Ls, N, P1, P).

zeros(P, P) --> [].
zeros(P0, P) --> [0],
        { P1 #= P0 + 1 },
        zeros(P1, P).

%?- matrix(4, 4, Ms), maplist(writeln, Ms).
