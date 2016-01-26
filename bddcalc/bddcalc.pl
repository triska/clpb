:- use_module(library(pio)).
:- use_module(library(clpb)).

:- set_prolog_flag(double_quotes, codes).

eval_actions([]) --> [].
eval_actions([A|As]) -->
        eval_action(A),
        eval_actions(As).

state(S), [S] --> [S].

state(S0, S), [S] --> [S0].

eval_action(ID=Expr) -->
        expr_value(Expr, Value),
        state(Assoc0, Assoc),
        { format("~w = ~w\n", [ID,Expr]),
          put_assoc(ID, Assoc0, Value, Assoc) }.
eval_action(taut(T)) -->
        state(Assoc),
        { get_assoc(T, Assoc, Value) },
        (   { Value == 1 } ->
            { format("~w is a tautology.\n", [T]) }
        ;   { throw(no_tautology(T,Value)) }
        ).

expr_value(id(ID), Value) -->
        state(Assoc0, Assoc),
        (   { get_assoc(ID, Assoc0, Value) } -> { Assoc = Assoc0 }
        ;   { clpb:enumerate_variable(V),
              clpb:make_node(V, 0, 1, Value),
              put_assoc(ID, Assoc0, Value, Assoc) }
        ).
expr_value(not(E), Value) -->
        expr_value(E, V),
        clpb:apply(#, 1, V, Value).
expr_value(xor(E1,E2), Value) -->
        expr_value(E1, V1),
        expr_value(E2, V2),
        clpb:apply(#, V1, V2, Value).
expr_value(nand(E1,E2), Value) --> expr_value(not(and(E1,E2)), Value).
expr_value(nor(E1,E2), Value) --> expr_value(not(or(E1,E2)), Value).
expr_value(or(E1,E2), Value) -->
        expr_value(E1, V1),
        expr_value(E2, V2),
        clpb:apply(+, V1, V2, Value).
expr_value(and(E1,E2), Value) -->
        expr_value(E1, V1),
        expr_value(E2, V2),
        clpb:apply(*, V1, V2, Value).
expr_value(biimp(E1,E2), Value) --> expr_value(xor(not(E1),E2), Value).


sample_calfile(1, 'c1355.cal').
sample_calfile(2, 'c1908.cal').
sample_calfile(3, 'c2670.cal').
sample_calfile(4, 'c3540.cal').
sample_calfile(5, 'c432.cal').
sample_calfile(6, 'c499.cal').

id(ID) -->
        [L],
        { (code_type(L, alpha) ; L = 0'_ ) },
        alnums(Ls),
        { atom_codes(ID, [L|Ls]) }.

alnums([A|As]) -->
        [A],
        { code_type(A, alnum) },
        alnums(As).
alnums([]) --> [].


run(File) :-
        phrase_from_file(calfile(As), File),
        empty_assoc(E),
        phrase(eval_actions(As), [E], [_]).


calfile(As) -->
        comment,
        ws,
        "initial", ..., ";", ws,
        "inputs", ..., ";", ws,
        "actions", ws,
        "autoreorder", ..., ";", ws,
        !,
        actions(As).

%?- phrase(action(A), "t4 = not t3;").

actions([A|As]) -->
        action(A),
        ws,
        !, % single solution: longest input match
        actions(As).
actions([]) --> [].

action(ID = Expr) --> id(ID), ws, "=", ws, expr(Expr), ws, ";".
action(taut(ID)) --> "tautology", ws, id(ID), ws, ";".

expr(E) --> factor(F), expr_r(F, E).

expr_r(E0, E) --> ws, binop(Op), ws, factor(E1), { E =.. [Op,E0,E1] }.
expr_r(E, E) --> [].

binop(xor)   --> "xor".
binop(nand)  --> "nand".
binop(nor)   --> "nor".
binop(or)    --> "or".
binop(biimp) --> "biimp".
binop(and)   --> "and".

factor(id(ID)) --> id(ID).
factor(not(E)) --> "not", ws, factor(E).

comment --> "/*", ..., "*/".
comment --> [].

... --> [] | [_], ... .

ws --> [W], { code_type(W, space) }, ws.
ws --> [].

