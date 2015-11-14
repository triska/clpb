:- use_module(clpb).

:- set_prolog_flag(clpb_validation, true).

sat(X, Y, X=:=Y).
sat(X, Y, X#Y).
sat(X, Y, X+Y).
sat(X, Y, X*Y).


vs_formula([V|Vs], F) :- vs_formula_(Vs, V, F).

vs_formula_([], F, F).
vs_formula_([V|Vs], F0, F) :-
        sat(V, F0, Sat),
        vs_formula_(Vs, Sat, F).

run :-
        sat(A, B, Sat1),
        sat(C, D, Sat2),
        sat(E, F, Sat3),
        sat(Sat1),
        sat(Sat2),
        sat(Sat3),
        vs_formula([A,B,C,D,E,F], Formula),
        sat(Formula),
        false.


%?- run.
