

CLP(B) is now included in SWI-Prolog as
[library(clpb)](http://www.swi-prolog.org/man/clpb.html).

Please see http://www.github.com/SWI-Prolog for the latest version.




This repository is still useful to keep track of test cases and usage
examples for the library.

There is also a limited alternative version using ZDDs:

   [zdd/clpb.pl](zdd/clpb.pl)

Try this version for tasks where the BDD-based version runs out of
memory. You must use `zdd_set_vars/1` before using `sat/1` though.
