/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Langford Sequence of order N: sequence of numbers 1,1,2,2,...,N,N
   such that the two occurrences of all k in 1..N are k units apart.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(clpb).
:- use_module(library(clpfd)).

%?- langford(8, Vs, Sat), time(sat_count(Sat, Count)).
%@ % 645,361,793 inferences, 76.585 CPU in 78.336 seconds (98% CPU, 8426737 Lips
%@ Count = 300,
%@ etc.

langford(N, Vs, *(Sats)) :-
        Len #= 3*N,
        length(Row, Len),
        findall(Row, (between(1,N,K), phrase(row(N,K), Row)), Matrix),
        transpose(Matrix, TMatrix),
        phrase(sats(TMatrix, Vs), Sats),
        zdd_set_vars(Vs).

sats([], _) --> [].
sats([Col|Cols], Vs0) -->
        { phrase(column_selection(Col, Vs0), Vs) },
        [card([1],Vs)],
        sats(Cols, Vs0).

column_selection([], []) --> [].
column_selection([C|Cs], [V|Vs]) -->
        (   { C =:= 1 } -> [V]
        ;   []
        ),
        column_selection(Cs, Vs).

row(N, K) -->
        n_zeros(_), [1], n_zeros(K), [1], n_zeros(_), % langford sequence
        { Prefix #= K - 1,                            % rest: represent K
          Suffix #= N - K },
        n_zeros(Prefix),
        [1],
        n_zeros(Suffix).

n_zeros(0)  --> [].
n_zeros(K0) --> [0], { K0 #> 0, K #= K0 - 1 }, n_zeros(K).

%?- length(Ls, 10), phrase(row(4, 3), Ls).
%@ Ls = [1, 0, 0, 0, 1, 0, 0, 0, 1, 0] ;
%@ Ls = [0, 1, 0, 0, 0, 1, 0, 0, 1, 0] ;
%@ false.
