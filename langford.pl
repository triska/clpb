/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Langford Sequence of order N: sequence of numbers 1,1,2,2,...,N,N
   such that the two occurrences of all k in 1..N are k units apart.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(between)).
:- use_module(library(dcgs)).

run :-
        length(_, N),
        langford(N, _, Sat),
        sat_count(Sat, Count),
        portray_clause(N=Count),
        false.

%?- run.
%@ 0=1.
%@ 1=1.
%@ 2=0.
%@ 3=2.
%@ 4=2.
%@ 5=0.
%@ 6=0.
%@ 7=52.

langford(N, Vs, *(Sats)) :-
        Len #= 3*N,
        length(Row, Len),
        findall(Row, (between(1,N,K), phrase(row(N,K), Row)), Matrix0),
        sort(Matrix0, Matrix),
        transpose(Matrix, TMatrix),
        phrase(sats(TMatrix, Vs), Sats).

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
