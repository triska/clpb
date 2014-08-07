:- use_module(clpb).

sat( _)  --> [].
sat(X*Y) --> [_], sat(X), sat(Y).
sat(X+Y) --> [_], sat(X), sat(Y).
sat(X#Y) --> [_], sat(X), sat(Y).

run(N) :-
        length(Ls, N),
        portray_clause(N),
        phrase(sat(Sat), Ls),
        term_variables(Sat, Vs0),
        sat(Sat),
        permutation(Vs0, Vs),
        labeling(Vs),
        false.

run :- run(_).

