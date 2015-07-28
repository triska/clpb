/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Domino tiling of an N x M chessboard.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(clpb).
:- use_module(library(clpfd)).


%?- run.

run :-
        length(_, N),
        time((dominos(N, N, _Vs, Conj), sat_count(Conj, Count),
              portray_clause(N=Count))),
        false.

%?- dominos(8, 8, Vs, Conj), sat(Conj).

%?- between(1,10, Cols), dominos(2, Cols, Vs, Conj), sat_count(Conj, N), writeln(Cols=N), false.
%@ 1=1
%@ 2=2
%@ 3=3
%@ 4=5
%@ 5=8
%@ 6=13
%@ 7=21
%@ 8=34
%@ 9=55
%@ 10=89
%@ false.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Monomino
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%tile([[1]]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Dominos
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tile([[1,1]]).

tile([[1],
      [1]]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Trominos
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% tile([[1,1],
%       [1,0]]).

% tile([[1,0],
%       [1,1]]).

% tile([[0,1],
%       [1,1]]).

% tile([[1,1],
%       [0,1]]).

% tile([[1,1,1]]).

% tile([[1],
%       [1],
%       [1]]).


dominos(M, N, Vs, *(Cs)) :-
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
        findall(Ls, line(N,Ls), Ms).


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
