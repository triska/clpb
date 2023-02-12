/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Partition the integers 1,...,N into 3 sum-free sets. A set is
   sum-free iff it does not contain i, j, and i+j.

   The Schur number for 3 is 13, so this is possible up to N = 13.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(time)).

%?- schur_ands_conj(13, _, Sat), time(taut(Sat, T)).
%@    % CPU time: 50.628s
%@    false.

%?- schur_ands_conj(14, _, Sat), time(taut(Sat, T)).
%@    % CPU time: 63.841s
%@    Sat = *(...), T = 0, ... .


schur_ands_conj(N, Ands, *(Ands)) :-
        length(Ls, N),
        maplist(same_length([_,_,_]), Ls),
        findall(triple(I,J,Sum), (between(1,N,I),
                                     between(I,N,J),
                                     Sum #= I+J, Sum #=< N), Triples),
        phrase((cards1(Ls),
                triples(Triples, Ls)), Ands).

triples([], _) --> [].
triples([T|Ts], Ls) --> triple(T, Ls), triples(Ts, Ls).

triple(triple(I,J,Sum), Ls) -->
        { nth1(I, Ls, As),
          nth1(J, Ls, Bs),
          nth1(Sum, Ls, Cs),
          transpose([As,Bs,Cs], Ts) },
        cards012(Ts).

cards1([]) --> [].
cards1([L|Ls]) --> [card([1],L)], cards1(Ls).

cards012([]) --> [].
cards012([L|Ls]) --> [card([0,1,2],L)], cards012(Ls).
