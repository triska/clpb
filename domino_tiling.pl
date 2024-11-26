/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Domino tiling of an M x N chessboard.
   Tested with Scryer Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(pairs)).
:- use_module(library(dcgs)).
:- use_module(library(time)).
:- use_module(library(between)).
:- use_module(library(reif)).

%?- run.

run :-
        length(_, N),
        time((dominoes(N, N, _Vs, Conj), sat_count(Conj, Count),
              portray_clause(N=Count))),
        false.

%?- dominoes(8, 8, Vs, Conj), sat_count(Conj, Count).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interestingly, the Fibonacci numbers arise when we ask for the
   number of domino tilings of a 2xN board.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%?- between(1,10,N), dominoes(2, N, Vs, Conj), sat_count(Conj, Count), portray_clause(N=Count), false.
%@ 1=1.
%@ 2=2.
%@ 3=3.
%@ 4=5.
%@ 5=8.
%@ 6=13.
%@ 7=21.
%@ 8=34.
%@ 9=55.
%@ 10=89.
%@    false.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Monomino
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%tile([[1]]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Dominoes
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tile([[1,1]]).

tile([[1],
      [1]]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Trominoes
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


dominoes(M, N, Vs, *(Cs)) :-
        matrix(M, N, Rows),
        same_length(Rows, Vs),
        transpose(Rows, Cols),
        phrase(all_cardinalities(Cols, Vs), Cs).

all_cardinalities([], _) --> [].
all_cardinalities([Col|Cols], Vs) -->
        { pairs_keys_values(Pairs0, Col, Vs),
          tfilter(key_one_t, Pairs0, Pairs),
          pairs_values(Pairs, Cs) },
        [card([1], Cs)],
        all_cardinalities(Cols, Vs).

key_one_t(Key-_, T) :- =(Key, 1, T).

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

%?- matrix(4, 4, Ms), maplist(portray_clause, Ms).
