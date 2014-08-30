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
        % foldl(or, Vs, 1, Or),
        % Or,
        findall(., label(Vs), Sols),
        length(Sols, C),
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
        diagonals(Qs, 1, 1, N).

row(Row) :-
        foldl(or, Row, 0, Or),
        Or,
        not_same_row(Row).

not_same_row([]).
not_same_row([Q|Qs]) :-
        not_same_row_(Qs, Q),
        not_same_row(Qs).

or(A, B, B #\/ A).

not_same_row_([], _).
not_same_row_([L|Ls], Q) :-
        Q #==> #\ L,
        not_same_row_(Ls, Q).

length_(L, Ls) :- length(Ls, L).

diagonals(Qs, Row, Col, N) :-
        (   Row > N -> true
        ;   Col > N ->
            Row1 is Row + 1,
            diagonals(Qs, Row1, 1, N)
        ;   queen_at(Qs, Row, Col, Q),
            DRow is Row + 1,
            DCol is Col + 1,
            diagonal_down(Qs, DRow, DCol, N, Q),
            URow is Row - 1,
            UCol is Col + 1,
            diagonal_up(Qs, URow, UCol, N, Q),
            Col1 is Col + 1,
            diagonals(Qs, Row, Col1, N)
        ).

diagonal_down(Qs, Row, Col, N,Q) :-
        (   Row > N -> true
        ;   Col > N -> true
        ;   queen_at(Qs, Row, Col, Q0),
            Q #==> #\ Q0,
            Row1 is Row + 1,
            Col1 is Col + 1,
            diagonal_down(Qs, Row1, Col1, N, Q)
        ).

diagonal_up(Qs, Row, Col, N, Q) :-
        (   Row < 1 -> true
        ;   Col > N -> true
        ;   queen_at(Qs, Row, Col, Q0),
            Q #==> #\ Q0,
            Row1 is Row - 1,
            Col1 is Col + 1,
            diagonal_up(Qs, Row1, Col1, N, Q)
        ).



queen_at(Qs, NRow, NCol, Q) :-
        nth1(NRow, Qs, Row),
        nth1(NCol, Row, Q).