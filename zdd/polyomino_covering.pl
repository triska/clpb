/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Polyomino tiling of an N x M chessboard.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(clpb).


%?- run.
%@ % 193 inferences, 0.000 CPU in 0.000 seconds (94% CPU, 2539474 Lips)
%@ 1=1.
%@ % 141,442 inferences, 0.026 CPU in 0.030 seconds (87% CPU, 5385600 Lips)
%@ 2=11.
%@ % 44,751 inferences, 0.005 CPU in 0.005 seconds (99% CPU, 9646691 Lips)
%@ 3=583.
%@ % 840,179 inferences, 0.094 CPU in 0.096 seconds (99% CPU, 8909073 Lips)
%@ 4=177332.
%@ % 8,620,032 inferences, 0.843 CPU in 0.850 seconds (99% CPU, 10224477 Lips)
%@ 5=329477745.
%@ % 74,118,376 inferences, 8.260 CPU in 8.311 seconds (99% CPU, 8973604 Lips)
%@ etc.

run :-
        length(_, N),
        time((polyominoes(N, N, Vs, Conj),
              zdd_set_vars(Vs),
              sat_count(Conj, Count),
              portray_clause(N=Count))),
        false.

%?- between(1,10, Cols), polyominoes(2, Cols, Vs, Conj), zdd_set_vars(Vs), sat_count(Conj, N), writeln(Cols=N), false.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Monomino
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tile([[1]]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Dominos
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tile([[1,1]]).

tile([[1],
      [1]]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Trominos
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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


polyominoes(M, N, Vs, *(Cs)) :-
        matrix(M, N, Ms),
        same_length(Ms, Vs),
        Ms = [First|_],
        phrase(all_cardinalities(First, Vs, Ms), Cs).

all_cardinalities([], _, _) --> [].
all_cardinalities([_|Rest], Vs, Rows0) -->
        { maplist(list_first_rest, Rows0, Fs, Rows),
          pairs_keys_values(Pairs0, Fs, Vs),
          include(key_one, Pairs0, Pairs),
          pairs_values(Pairs, Cs) },
        [card([1], Cs)],
        all_cardinalities(Rest, Vs, Rows).

key_one(1-_).

list_first_rest([L|Ls], L, Ls).


matrix(M, N, Ms) :-
        Squares is M*N,
        length(Ls, Squares),
        findall(Ls, line(N,Ls), Ms0),
        sort(Ms0, Ms1),
        reverse(Ms1, Ms).


line(N, Ls) :-
        tile(Ts),
        length(Ls, Max),
        phrase((zeros(0,P0),tile_(Ts,N,Max,P0,P1),zeros(P1,_)), Ls).

tile_([], _, _, P, P) --> [].
tile_([T|Ts], N, Max, P0, P) -->
        tile_part(T, N, P0, P1),
        { (P1 - 1) mod N >= P0 mod N,
          P2 is min(P0 + N, Max) },
        zeros(P1, P2),
        tile_(Ts, N, Max, P2, P).

tile_part([], _, P, P) --> [].
tile_part([L|Ls], N, P0, P) -->
        [L],
        { P1 is P0 + 1 },
        tile_part(Ls, N, P1, P).

zeros(P, P) --> [].
zeros(P0, P) --> [0],
        { P1 is P0 + 1 },
        zeros(P1, P).

%?- matrix(4, 4, Ms), maplist(writeln, Ms).
