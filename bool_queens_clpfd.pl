:- use_module(library(clpfd)).

%?- run.
%@ |queens(0)| = 1           after 0.00s
%@ |queens(1)| = 1           after 0.00s
%@ |queens(2)| = 0           after 0.00s
%@ |queens(3)| = 0           after 0.01s
%@ |queens(4)| = 2           after 0.02s
%@ |queens(5)| = 10          after 0.05s
%@ |queens(6)| = 4           after 0.11s
%@ |queens(7)| = 40          after 0.26s
%@ |queens(8)| = 92          after 0.72s
%@ |queens(9)| = 352         after 2.52s
%@ |queens(10)| = 724        after 9.74s

%?- n_queens(4, Qs), append(Qs, Vs), label(Vs), maplist(writeln, Qs).
%@ [0,0,1,0]
%@ [1,0,0,0]
%@ [0,0,0,1]
%@ [0,1,0,0]
%@ Qs = [[0, 0, 1, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0]],
%@ Vs = [0, 0, 1, 0, 1, 0, 0, 0, 0|...] .

run :-
        length(_, N),
        statistics(cputime, T0),
        n_queens(N, Qs),
        append(Qs, Vs),
        findall(., label(Vs), Sols),
        length(Sols, C),
        statistics(cputime, T1),
        Time is T1 - T0,
        format("|queens(~w)| = ~w  ~25| after ~2fs\n", [N,C,Time]),
        false.

n_queens(N, Qs) :-
        length(Qs, N),
        maplist(length_(N), Qs),
        maplist(one_in_row, Qs),
        maplist(row, Qs),
        transpose(Qs, TQs),
        maplist(one_in_row, TQs),
        maplist(row, TQs),
        phrase(diagonals(Qs, 1, 1, N), Ands),
        foldl(and, Ands, 1#/\1, And),
        And.

one_in_row(Row) :-
        foldl(or, Row, 0#\/0, Or),
        Or.

row(Row) :-
        phrase(not_same_row(Row), Ands),
        foldl(and, Ands, 1#/\1, And),
        And.

and(A, B, B #/\ A).

not_same_row([]) --> [].
not_same_row([Q|Qs]) -->
        not_same_row_(Qs, Q),
        not_same_row(Qs).

or(A, B, B #\/ A).

not_same_row_([], _) --> [].
not_same_row_([L|Ls], Q) -->
        not_same(Q, L),
        not_same_row_(Ls, Q).


not_same(A, B) --> [#\ ( A #/\ B)].
%not_same(A, B) --> { #\ (A #/\ B) }.

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
            not_same(Q, Q0),
            { Row1 #= Row + 1,
              Col1 #= Col + 1 },
            diagonal_down(Qs, Row1, Col1, N, Q)
        ).

diagonal_up(Qs, Row, Col, N, Q) -->
        (   { Row < 1 } -> []
        ;   { Col > N } -> []
        ;   { queen_at(Qs, Row, Col, Q0) },
            not_same(Q, Q0),
            { Row1 #= Row - 1,
              Col1 #= Col + 1 },
            diagonal_up(Qs, Row1, Col1, N, Q)
        ).



queen_at(Qs, NRow, NCol, Q) :-
        nth1(NRow, Qs, Row),
        nth1(NCol, Row, Q).