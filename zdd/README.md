
# ZDD-based variant of `library(clpb)`

Zero-suppressed Binary Decision Diagrams (ZDDs) can save a lot of
memory when most Boolean variables are equal to&nbsp;0 in solutions.
This is the case in many covering tasks.

[**clpb.pl**](clpb.pl) is a ZDD-based variant of `library(clpb)`. It
is mostly a drop-in-replacement for the library that ships with
Scryer&nbsp;Prolog, with the following important difference: Before using
`sat/1`, you *must* call `zdd_set_vars/1` with a list of all Boolean
variables that occur in your model.

## Examples

### Polyomino tilings

This variant of the library can be used to more space-efficiently
compute that there are exactly
92,109,458,286,284,989,468,604&nbsp;ways to cover an
8&times;8&nbsp;chessboard with monominoes, dominoes and trominoes. See
[**polyomino_tiling.pl**](polyomino_tiling.pl) for more information.

In addition, as with `library(clpb)`, solutions can also be picked in
such a way that each solution is equally likely.

Sample solution:

![](../figures/filler.png) ![Polyomino tiling of an 8x8 chessboard](../figures/polyomino8x8.png)

**Exercises**:

- A chessboard tiling is *faultfree* if every straight line that
  passes through the interior of the board also passes through the
  interior of some domino. Add suitable constraints to describe
  solutions that are faultfree.

- How many of the above solutions satisfy the additional property that
  *no two congruent pieces are adjacent*?

- And of those, how many are there where congruent pieces touch at
  their corners?

### Project Euler Problem 161

Project Euler [Problem 161](https://projecteuler.net/problem=161) asks
for the number of *Triomino&nbsp;tilings* of a 9&times;12 grid.

[**euler_161.pl**](euler_161.pl) shows how this can be solved with the
ZDD-based variant of CLP(B)&nbsp;constraints. Using an Intel
Core&nbsp;i7 CPU (2.67&nbsp;GHz), you know after a few&nbsp;days of
computation&nbsp;time: There are exactly
20,574,308,184,277,971&nbsp;ways to do&nbsp;it.

One of these solutions, picked at random:

![](../figures/filler.png) ![Triominoe tiling of a 9x12 grid](../figures/euler_161.png)

## Limitations

There are currently some limitations:

- unification of CLP(B) variables is *not* yet implemented in this variant
- `labeling/1` does *not* work yet.

In addition, `card/2` does not yet support integer ranges.

Please see the [source file](clpb.pl) for more information about these
issues.
