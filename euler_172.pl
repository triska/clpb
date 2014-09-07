
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Project Euler, Problem 172:

   How many 18-digit numbers n (without leading zeros) are there such
   that no digit occurs more than three times in n?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(clpb).
:- use_module(library(clpfd)).


len_num(L, N) :-
        length(Digits, L),
        maplist(length_list(10), Digits),
        Digits = [First|_],
        First = [0|_],          % no leading zero
        maplist(card([1]), Digits),
        transpose(Digits, DigitsT),
        maplist(card([0,1,2,3]), DigitsT),
        append(Digits, Vs),
        foldl(or, Vs, 1, Or),
        sat_count(Or, N).

or(A, B, B+A).

card(C, Ls) :- sat(card(C, Ls)).

length_list(L, Ls) :- length(Ls, L).

run(L) :-
        len_num(L, N),
        portray_clause(len_num(L,N)).

run :-
        length(_, N),
        time(run(N)),
        false.

%?- run.
%@ % 9 inferences, 0.000 CPU in 0.000 seconds (77% CPU, 692308 Lips)
%@ len_num(1, 9).
%@ % 363,166 inferences, 0.091 CPU in 0.093 seconds (98% CPU, 3978070 Lips)
%@ len_num(2, 90).
%@ % 546,422 inferences, 0.097 CPU in 0.097 seconds (100% CPU, 5654669 Lips)
%@ len_num(3, 900).
%@ % 884,158 inferences, 0.159 CPU in 0.160 seconds (100% CPU, 5546579 Lips)
%@ len_num(4, 8991).
%@ % 1,545,390 inferences, 0.276 CPU in 0.277 seconds (100% CPU, 5597880 Lips)
%@ len_num(5, 89586).
%@ % 3,224,186 inferences, 0.598 CPU in 0.601 seconds (100% CPU, 5390389 Lips)
%@ len_num(6, 888570).
%@ % 8,698,649 inferences, 1.537 CPU in 1.545 seconds (99% CPU, 5660084 Lips)
%@ len_num(7, 8754480).
