:- use_module(library(clpb)).
:- use_module(library(clpfd)).

%?- run.
%@ |queens(0)| = 1           after 0.00s
%@ |queens(1)| = 1           after 0.00s
%@ |queens(2)| = 0           after 0.00s
%@ |queens(3)| = 0           after 0.01s
%@ |queens(4)| = 2           after 0.12s
%@ |queens(5)| = 10          after 0.88s
%@ |queens(6)| = 4           after 6.69s
%@ |queens(7)| = 40          after 39.58s
%@ etc.


%?- n_queens(4, Qs, _, Sat), sat(Sat), append(Qs, Vs), labeling(Vs), maplist(writeln, Qs).
%@ [0,0,1,0]
%@ [1,0,0,0]
%@ [0,0,0,1]
%@ [0,1,0,0]
%@ etc.

run :-
        length(_, N),
        statistics(cputime, T0),
        n_queens(N, Qs, _, Sat),
        append(Qs, Vs),
        (   sat(Sat) ->
            sat_count(+[1|Vs], C)
        ;   C = 0
        ),
        statistics(cputime, T1),
        Time is T1 - T0,
        format("|queens(~w)| = ~w  ~25| after ~2fs\n", [N,C,Time]),
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
        (   { Row > N } -> []
        ;   { Col > N } ->
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
        (   { Row > N } -> []
        ;   { Col > N } -> []
        ;   { queen_at(Qs, Row, Col, Q0) },
            [~Q + ~Q0],
            { Row1 #= Row + 1,
              Col1 #= Col + 1 },
            diagonal_down(Qs, Row1, Col1, N, Q)
        ).

diagonal_up(Qs, Row, Col, N, Q) -->
        (   { Row < 1 } -> []
        ;   { Col > N } -> []
        ;   { queen_at(Qs, Row, Col, Q0) },
            [~Q + ~Q0],
            { Row1 #= Row - 1,
              Col1 #= Col + 1 },
            diagonal_up(Qs, Row1, Col1, N, Q)
        ).



queen_at(Qs, NRow, NCol, Q) :-
        nth1(NRow, Qs, Row),
        nth1(NCol, Row, Q).
