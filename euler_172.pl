
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Project Euler, Problem 172:

   How many 18-digit numbers n (without leading zeros) are there such
   that no digit occurs more than three times in n?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(time)).


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
%@    % CPU time: 0.000s
%@ len_num(1,9).
%@    % CPU time: 0.005s
%@ len_num(2,90).
%@    % CPU time: 0.015s
%@ len_num(3,900).
%@    % CPU time: 0.046s
%@ len_num(4,8991).
%@    % CPU time: 0.155s
%@ len_num(5,89586).
%@    % CPU time: 0.488s
%@ len_num(6,888570).
%@    % CPU time: 1.401s
%@ len_num(7,8754480).
%@    % CPU time: 3.751s
%@ len_num(8,85480920).
%@    % CPU time: 9.623s
