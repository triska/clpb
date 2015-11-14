### `library(clpb)` ships with SWI-Prolog

CLP(B), Constraint Logic Programming over Boolean variables, is
available in SWI-Prolog as
[**library(clpb)**](http://www.swi-prolog.org/man/clpb.html).

This repository contains usage examples and tests of the library.

### Using CLP(B) constraints

I recommend you study the following examples in particular:

1. [**knights_and_knaves.pl**](knights_and_knaves.pl): Solution of
   several Boolean puzzles that appear in Raymond Smullyan's _What Is
   the Name of this Book_ and Maurice Kraitchik's _Mathematical
   Recreations_. A good starting point for learning more about CLP(B).

2. [**matchsticks.pl**](matchsticks.pl): A puzzle involving
   matchsticks. See below for more information.

3. [**cycle_n.pl**](cycle_n.pl): Expresses independent sets and
   maximal independent sets (also called *kernels*) of the
   [cycle graph](https://en.wikipedia.org/wiki/Cycle_graph)&nbsp;C<sub>N</sub>.
   
   ![Cycle graph C_7](figures/cycle7.png) ![Kernel of C_7](figures/cycle7_kernel.png)

    See below for more information about weighted solutions.

4. [**euler_172.pl**](euler_172.pl): CLP(B) solution of Project Euler
   [Problem 172](https://projecteuler.net/problem=172): How many
   18-digit numbers&nbsp;<i>n</i> (without leading zeros) are there
   such that no digit occurs more than three times in&nbsp;<i>n</i>?

Other examples are useful as benchmarks:
[bool_queens.pl](bool_queens.pl), [langford.pl](langford.pl) and
[schur.pl](schur.pl).

#### Matchsticks puzzle

In [matchsticks.pl](matchsticks.pl), Boolean variables indicate
whether a matchstick is placed at a specific position. The task is to
eliminate all subsquares from the initial configuration in such a way
that the maximum number of matchsticks is left in place:

![Matchsticks initial configuration](figures/matchsticks1.png)

We can use the CLP(B) predicate `weighted_maximum/3` to show that we
need to remove at least 9 matchsticks to eliminate all subsquares.

![Matchsticks without any subsquares](figures/matchsticks2.png) ![Exactly 7 subsquares remaining](figures/matchsticks3.png)

The left figure shows a sample solution, leaving the maximum number of
matchsticks&nbsp;(31) in place. If you keep more matchsticks in place,
subsquares will remain. For example, the right figure contains exactly
7&nbsp;subsquares, including the 4x4 outer square.

CLP(B) constraints can be used to quickly generate, test and count
solutions of such puzzles, among many other applications. For example,
there are precisely 62,382,215,032 subsquare-free configurations that
use exactly 18&nbsp;matchsticks. This is the maximum number of such
configurations for any fixed number of matchsticks on this grid.

#### Independent sets and weighted kernels

As another example, consider the following graph:

![Cycle graph with 100 nodes, C_100](figures/cycle100.png)

It is the so-called
[_cycle graph_](https://en.wikipedia.org/wiki/Cycle_graph) with
100&nbsp;nodes, C<sub>100</sub>. Using CLP(B) constraints, it is easy
to see that this graph has exactly 792,070,839,848,372,253,127
_independent sets_, and exactly 1,630,580,875,002 _maximal_
independent sets, which are also called _kernels_. The gray nodes in
the next picture show one such kernel:

![Maximal independent set of C_100](figures/cycle100_maximum.png)

This is also a kernel of _maximum weight_ if the weight of each node
is its _Thue-Morse code_ (see [cycle_n.pl](cycle_n.pl) for more
details). Nodes with negative weights are drawn as squares.

Only 5 nodes (1, 25, 41, 73 and 97) of this kernel with 38 nodes have
negative weights in this case, for a total weight of 28. There are
exactly 256 kernels of maximum weight in this case. There are exactly
25,446,195,000 kernels with exactly 38 nodes. All kernels have between
34 and 50 nodes. For any fixed number of nodes, the maximum number of
kernels (492,957,660,000) is attained with 41 nodes, and among these
kernels, the maximum total Thue-Morse weight is 25.

By negating the coefficients of `maximum_weight/3`, we can also find
kernels with _minimum_ weight. For example:

![Kernel of C_100 with minimum weight](figures/cycle100_minimum.png)

### Alternative ZDD-based version of `library(clpb)`

There is a limited alternative version of `library(clpb)`, based on
Zero-suppressed Binary Decision Diagrams (ZDDs).

Please see the [**zdd**](zdd) directory for more information. Try the
ZDD-based version for tasks where the BDD-based version runs out of
memory. You must use `zdd_set_vars/1` before using `sat/1` though.
