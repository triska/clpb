:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(time)).
:- use_module(library(format)).

%?- run.
%@ |queens(0)| = 1           after 0.00s
%@ |queens(1)| = 1           after 0.00s
%@ |queens(2)| = 0           after 0.00s
%@ |queens(3)| = 0           after 0.08s
%@ |queens(4)| = 2           after 0.55s
%@ |queens(5)| = 10          after 4.20s
%@ |queens(6)| = 4           after 34.45s
%@ |queens(7)| = 40          after 224.07s
%@ etc.


%?- n_queens(4, Qs, _, Sat), sat(Sat), append(Qs, Vs), labeling(Vs), maplist(portray_clause, Qs).
%@ [0,0,1,0]
%@ [1,0,0,0]
%@ [0,0,0,1]
%@ [0,1,0,0]
%@ etc.

:- use_module(library(debug)).
run :-
        length(_, N),
        statistics(runtime, [T0,_]),
        n_queens(N, Qs, _, Sat),
        append(Qs, Vs),
        (   sat(Sat) ->
            sat_count(+[1|Vs], C)
        ;   C = 0
        ),
        statistics(runtime, [T1|_]),
        Time is T1 - T0,
        format("|queens(~w)| = ~w ~t~25| after ~2fs\n", [N,C,Time/1000]),
        false.

n_queens(N, Qs, Ands, *(Ands)) :-
        length(Qs, N),
        maplist(length_(N), Qs),
        transpose(Qs, TQs),
        phrase((rows(Qs),rows(TQs),
                diagonals(Qs, 1, 1, N)), Ands).

rows([]) --> [].
rows([Row|Rows]) -->
        [+Row],
        not_same_row(Row),
        rows(Rows).

not_same_row([]) --> [].
not_same_row([Q|Qs]) -->
        not_same_row_(Qs, Q),
        not_same_row(Qs).

not_same_row_([], _) --> [].
not_same_row_([L|Ls], Q) -->
        [~Q + ~L],
        not_same_row_(Ls, Q).

length_(L, Ls) :- length(Ls, L).

diagonals(Qs, Row, Col, N) -->
        (   { Row #> N } -> []
        ;   { Col #> N } ->
            { Row1 #= Row + 1 },
            diagonals(Qs, Row1, 1, N)
        ;   { queen_at(Qs, Row, Col, Q),
              DRow #= Row + 1,
              DCol #= Col + 1 },
            diagonal_down(Qs, DRow, DCol, N, Q),
            { URow #= Row - 1,
              UCol #= Col + 1 },
            diagonal_up(Qs, URow, UCol, N, Q),
            { Col1 #= Col + 1 },
            diagonals(Qs, Row, Col1, N)
        ).

diagonal_down(Qs, Row, Col, N,Q) -->
        (   { Row #> N } -> []
        ;   { Col #> N } -> []
        ;   { queen_at(Qs, Row, Col, Q0) },
            [~Q + ~Q0],
            { Row1 #= Row + 1,
              Col1 #= Col + 1 },
            diagonal_down(Qs, Row1, Col1, N, Q)
        ).

diagonal_up(Qs, Row, Col, N, Q) -->
        (   { Row #< 1 } -> []
        ;   { Col #> N } -> []
        ;   { queen_at(Qs, Row, Col, Q0) },
            [~Q + ~Q0],
            { Row1 #= Row - 1,
              Col1 #= Col + 1 },
            diagonal_up(Qs, Row1, Col1, N, Q)
        ).



queen_at(Qs, NRow, NCol, Q) :-
        nth1(NRow, Qs, Row),
        nth1(NCol, Row, Q).
