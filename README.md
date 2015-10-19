

CLP(B) is now included in SWI-Prolog as
[library(clpb)](http://www.swi-prolog.org/man/clpb.html).

Please see http://www.github.com/SWI-Prolog for the latest version.

This repository contains usage examples and tests of the library.

**Example** of Boolean variables ([matchsticks.pl](matchsticks.pl)):
Indicate whether a matchstick should be placed at a specific position.

![Matchsticks initial configuration](matchsticks1.png)

We can use the CLP(B) predicate `weighted_maximum/3` to show that we
need to remove at least 9 matchsticks to eliminate all subsquares.
Sample solution, leaving the maximum number of matchsticks in place:

![Matchsticks without any subsquares](matchsticks2.png)

Note that subsquares will remain of you keep more matchsticks in
place. For example, the following configuration contains precisely 7
subsquares, including the 4x4 outer square:

![Precisely 7 subsquares remaining](matchsticks3.png)

CLP(B) constraints can be used to quickly generate, test and count
solutions of such puzzles, among many other applications. For example,
there are precisely 62,382,215,032 subsquare-free configurations that
use exactly 18 matchsticks. This is the maximum number of such
configurations for any fixed number of matchsticks on this roster.

### Alternative ZDD-based version of `library(clpb)`

There is a limited alternative version of `library(clpb)`, based on
Zero-suppressed Binary Decision Diagrams (ZDDs).

Please see the [zdd](zdd) directory for more information. Try the
ZDD-based version for tasks where the BDD-based version runs out of
memory. You must use `zdd_set_vars/1` before using `sat/1` though.
