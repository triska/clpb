/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Project Euler Problem 161:
   Number of Trominos tilings of a 9x12 board.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(clpb).
:- use_module(library(clpfd)).

euler_161(Count) :-
        polyominos(9, 12, Vs, Conj),
        zdd_set_vars(Vs),
        sat_count(Conj, Count).

%?- between(1,10, Cols), polyominos(2, Cols, Vs, Conj), zdd_set_vars(Vs), sat_count(Conj, N), writeln(Cols=N), false.


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


polyominos(M, N, Vs, *(Cs)) :-
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
