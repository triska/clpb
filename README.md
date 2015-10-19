

CLP(B) is now included in SWI-Prolog as
[library(clpb)](http://www.swi-prolog.org/man/clpb.html).

Please see http://www.github.com/SWI-Prolog for the latest version.

This repository contains usage examples and tests of the library.

**Example** of Boolean variables ([matchsticks.pl](matchsticks.pl)):
Indicate whether a matchstick should be placed at a specific position.

![Matchsticks initial configuration](matchsticks1.png)

We can use the CLP(B) predicate `weighted_minimum/3` to show that we
need to remove at least 9 matchsticks to eliminate all sub-squares.
Sample solution, leaving the maximum number of matchsticks in place:

![Matchsticks without any subsquares](matchsticks2.png)

There is also a limited alternative version using ZDDs. Please see the
[zdd](zdd) directory for more information. Try the ZDD-based version
for tasks where the BDD-based version runs out of memory. You must use
`zdd_set_vars/1` before using `sat/1` though.

