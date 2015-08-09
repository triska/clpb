/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Partition the integers 1,...,N into 3 sum-free set (no set contains
   the numbers i, j, and i+j).

   The Schur number for 3 is 13, so this is possible up to N = 13.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpb)).
:- use_module(library(clpfd)).

%?- schur_ands_conj(13, _, Sat), time(taut(Sat, T)).
%@ % 90,526,221 inferences, 9.416 CPU in 9.447 seconds (100% CPU, 9613948 Lips)
%@ false.

%?- schur_ands_conj(14, _, Sat), time(taut(Sat, T)).
%@ % 37,741,219 inferences, 5.395 CPU in 5.422 seconds (100% CPU, 6995505 Lips)
%@ T = 0,
%@ etc.


list_conj([], 1).
list_conj([L|Ls], L*Conj) :- list_conj(Ls, Conj).


schur_ands_conj(N, Ands, Sat) :-
        length(Ls, N),
        maplist(same_length([_,_,_]), Ls),
        findall(triple(I,J,Sum), (between(1,N,I),
                                     between(I,N,J),
                                     Sum #= I+J, Sum #=< N), Triples),
        phrase((cards1(Ls),
                triples(Triples, Ls)), Ands),
        list_conj(Ands, Sat).

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
