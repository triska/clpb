:- use_module(clpb).

sat(_)  --> [].
sat(X*Y) --> [_], sat(X), sat(Y).
sat(X+Y) --> [_], sat(X), sat(Y).
sat(X#Y) --> [_], sat(X), sat(Y).


%?- vs_eqs([A,B,C,D], Eqs).

vs_eqs(Vs, Eqs) :- phrase(vs_eqs(Vs), Eqs).

vs_eqs([]) --> [].
vs_eqs([V|Vs]) --> vs_eqs_(Vs, V), vs_eqs(Vs).

vs_eqs_([], _) --> [].
vs_eqs_([V|Vs], X) --> vs_eqs_(Vs, X), ( [X=V] ; [] ).

run(N) :-
        length(Ls, N),
        portray_clause(N),
        phrase(sat(Sat1), Ls),
        phrase(sat(Sat2), Ls),
        term_variables(Sat1-Sat2, Vs0),
        permutation(Vs0, Vs),
        vs_eqs(Vs, Eqs),
        findall(Vs, (sat(Sat1),sat(Sat2),maplist(call, Eqs),labeling(Vs)), Sols1),
        findall(Vs, (labeling(Vs),maplist(call,Eqs),sat(Sat1*Sat2)), Sols2),
        (   sort(Sols1, Sols), sort(Sols2, Sols) -> true
        ;   throw(neq-Sat1-Sat2-Eqs-Vs0-Vs-Sols1-Sols2)
        ),
        % (   Sols1 == [] ->
        %     (   \+ \+ (maplist(call,Eqs), taut(Sat, T), T == 0) -> true
        %     ;   throw(tautfail-Eqs-Sat)
        %     )
        % ;   true
        % ),
        false.

run :- run(_).

%?- run.
