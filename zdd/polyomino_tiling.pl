/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Polyomino tiling of an N x M chessboard.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(clpb).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(dcgs)).
:- use_module(library(reif)).
:- use_module(library(time)).
:- use_module(library(format)).
:- use_module(library(between)).


%?- run.
%@ 0=1.
%@    % CPU time: 0.000s, 552 inferences
%@ 1=1.
%@    % CPU time: 0.000s, 1_909 inferences
%@ 2=11.
%@    % CPU time: 0.013s, 63_662 inferences
%@ 3=583.
%@    % CPU time: 0.240s, 1_179_806 inferences
%@ 4=177332.
%@    % CPU time: 2.343s, 11_429_112 inferences
%@ 5=329477745.
%@    % CPU time: 18.502s, 91_996_815 inferences

run :-
        length(_, N),
        time((polyominoes(N, N, Vs, Conj),
              zdd_set_vars(Vs),
              sat_count(Conj, Count),
              portray_clause(N=Count))),
        false.

%?- between(1,10, Cols), polyominoes(2, Cols, Vs, Conj), zdd_set_vars(Vs), sat_count(Conj, N), portray_clause(Cols=N), false.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Monomino
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tile([[1]]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Dominoes
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tile([[1,1]]).

tile([[1],
      [1]]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Trominoes
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

key_one_t(K-_, T) :- =(K, 1, T).


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

%?- matrix(4, 4, Ms), maplist(portray_clause, Ms).
