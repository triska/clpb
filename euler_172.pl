
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
        transpose(Digits, DigitsT),
        % the order of the following 2 constraints has a huge impact
        % on performance!
        maplist(card([0,1,2,3]), DigitsT),
        maplist(card([1]), Digits),
        append(Digits, Vs),
        sat_count(+[1|Vs], N).

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
%@ % 9 inferences, 0.000 CPU in 0.000 seconds (73% CPU, 642857 Lips)
%@ len_num(1, 9).
%@ % 130,209 inferences, 0.041 CPU in 0.058 seconds (72% CPU, 3161103 Lips)
%@ len_num(2, 90).
%@ % 33,481 inferences, 0.006 CPU in 0.006 seconds (99% CPU, 5549644 Lips)
%@ len_num(3, 900).
%@ % 100,992 inferences, 0.019 CPU in 0.033 seconds (58% CPU, 5261096 Lips)
%@ len_num(4, 8991).
%@ % 364,252 inferences, 0.069 CPU in 0.108 seconds (64% CPU, 5292514 Lips)
%@ len_num(5, 89586).
%@ % 1,243,083 inferences, 0.226 CPU in 0.273 seconds (83% CPU, 5490504 Lips)
%@ len_num(6, 888570).
%@ % 3,805,379 inferences, 0.687 CPU in 0.710 seconds (97% CPU, 5539488 Lips)
%@ len_num(7, 8754480).
%@ % 10,804,441 inferences, 1.998 CPU in 2.333 seconds (86% CPU, 5408546 Lips)
