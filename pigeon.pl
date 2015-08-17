/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Pigeon I x J: Place j pigeons into i holes, such that each hole
   holds at most one pigeon.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpb)).
:- use_module(library(clpfd)).

%?- pigeon(8, 8, Rows, _, Conj), sat(Conj), maplist(labeling, Rows), maplist(writeln, Rows).
%@ [0,0,0,0,0,0,0,1]
%@ [0,0,0,0,0,0,1,0]
%@ [0,0,0,0,0,1,0,0]
%@ [0,0,0,0,1,0,0,0]
%@ [0,0,0,1,0,0,0,0]
%@ [0,0,1,0,0,0,0,0]
%@ [0,1,0,0,0,0,0,0]
%@ [1,0,0,0,0,0,0,0]

%?- pigeon(8, 9, _, _, Conj), sat(Conj).
%@ false.

pigeon(I, J, Rows, Ls, *(Ls)) :-
        length(Rows, J),
        maplist(length_list(I), Rows),
        transpose(Rows, TRows),
        phrase((all_card1(Rows),all_max1(TRows)), Ls).

length_list(N, Ls) :- length(Ls, N).

all_card1([]) --> [].
all_card1([Ls|Lss]) --> [card([1],Ls)], all_card1(Lss).

all_max1([]) --> [].
all_max1([Ls|Lss]) --> [card([0,1],Ls)], all_max1(Lss).
