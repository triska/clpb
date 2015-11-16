/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Expressing XOR with 4 NAND gates.

   Using universally quantified variables, you can use CLP(B)
   constraints to show that the circuit computes XOR as intended.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(clpb)).

nand_gate(X, Y, Z) :- sat(Z =:= ~(X*Y)).

xor(X, Y, Z) :-
        nand_gate(X, Y, T1),
        nand_gate(X, T1, T2),
        nand_gate(Y, T1, T3),
        nand_gate(T2, T3, Z).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ?- xor(X, Y, Z).
   %@ sat(X=:=Y#Z).

   ?- xor(x, y, Z).
   %@ sat(Z=:=x#y).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */