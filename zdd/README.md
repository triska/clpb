
This is a ZDD-based variant of `library(clpb)`.

ZDDs can save a lot of memory when most Boolean variables are equal
to&nbsp;0 in solutions. This is the case in many covering tasks.

For example, this variant of the library can be used to more
space-efficiently compute that there are exactly
92,109,458,286,284,989,468,604&nbsp;ways to cover an 8x8 chessboard
with monominos, dominos and trominos. See
[polyomino_covering.pl](polyomino_covering.pl) for more information.

In addition, solutions can be picked in such a way that each solution
is equally likely.

Sample solution:

![Polyomino covering](polyomino_covering.png)

The ZDD-based version ([clpb.pl](clpb.pl)) is mostly a
drop-in-replacement for the version that ships with SWI-Prolog, with
the following important difference: Before using `sat/1`, you *must*
call `zdd_set_vars/1` with a list of all Boolean variables that occur
in your model.

Please see the source file for more information.
