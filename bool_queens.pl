:- use_module(clpb).
:- use_module(library(clpfd)).

%?- run.
%@ |queens(0)| = 1           after 0.00s
%@ |queens(1)| = 1           after 0.00s
%@ |queens(4)| = 2           after 0.04s
%@ |queens(5)| = 10          after 0.25s
%@ |queens(6)| = 4           after 1.02s
%@ |queens(7)| = 40          after 5.90s
%@ |queens(8)| = 92          after 30.51s


%?- n_queens(4, Qs), append(Qs, Vs), labeling(Vs), maplist(writeln, Qs).
%@ [0,0,1,0]
%@ [1,0,0,0]
%@ [0,0,0,1]
%@ [0,1,0,0]
%@ Qs = [[0, 0, 1, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0]],
%@ Vs = [0, 0, 1, 0, 1, 0, 0, 0, 0|...] .

run :-
        length(_, N),
        statistics(cputime, T0),
        length(Qs, N),
        maplist(length_(N), Qs),
        append(Qs, Vs),
        % sat(+[1|Vs]), % force variables into same BDD
        n_queens(N, Qs),
        append(Qs, Vs),
        sat_count(+[1|Vs], C),
        statistics(cputime, T1),
        Time is T1 - T0,
        format("|queens(~w)| = ~w  ~25| after ~2fs\n", [N,C,Time]),
        false.

n_queens(N, Qs) :-
        length(Qs, N),
        maplist(length_(N), Qs),
        maplist(row, Qs),
        transpose(Qs, TQs),
        maplist(row, TQs),
        phrase(diagonals(Qs, 1, 1, N), Ands),
        maplist(at_most_one, Ands).

at_most_one(Ls) :- sat(card([0,1], Ls)).

row(Row) :- sat(card([1], Row)).

length_(L, Ls) :- length(Ls, L).

diagonals(Qs, Row, Col, N) -->
        (   { Row > N } -> []
        ;   { Col > N } ->
            { Row1 is Row + 1 },
            diagonals(Qs, Row1, 1, N)
        ;   { phrase(diagonal_down(Qs, Row, Col, N), Ds) },
            [Ds],
            { phrase(diagonal_up(Qs, Row, Col, N), Us) },
            [Us],
            { Col1 is Col + 1 },
            diagonals(Qs, Row, Col1, N)
        ).

diagonal_down(Qs, Row, Col, N) -->
        (   { Row > N } -> []
        ;   { Col > N } -> []
        ;   queen_at(Qs, Row, Col),
            { Row1 is Row + 1,
              Col1 is Col + 1 },
            diagonal_down(Qs, Row1, Col1, N)
        ).

diagonal_up(Qs, Row, Col, N) -->
        (   { Row < 1 } -> []
        ;   { Col > N } -> []
        ;   queen_at(Qs, Row, Col),
            { Row1 is Row - 1,
              Col1 is Col + 1 },
            diagonal_up(Qs, Row1, Col1, N)
        ).


queen_at(Qs, NRow, NCol) -->
        { nth1(NRow, Qs, Row),
          nth1(NCol, Row, Q) },
        [Q].
