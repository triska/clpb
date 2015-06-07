
This is a ZDD-based variant of `library(clpb)`.

ZDDs can save a lot of memory in situations where solutions have most
Boolean variables equal to 0. This is the case in many covering tasks.

For example, this variant of the library can be used to more
space-efficiently compute that there are
92109458286284989468604&nbsp;ways to cover an 8x8 chessboard with
monominos, dominos and trominos. See
[polyomino_covering.pl](polyomino_covering.pl) for more information.

In addition, solutions can be picked in such a way that each solution
is equally likely.

Sample solution:

![Polyomino covering](polyomino_covering.png)

[clpb.pl] is mostly a drop-in-replacement for the version that ships
with SWI-Prolog, with the following important difference: Before using
`sat/1`, you *must* call `zdd_set_vars/1` with a list of all Boolean
variables that occur in your model.

Please see the source file for more information.
