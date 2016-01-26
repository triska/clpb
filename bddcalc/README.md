## Simple BDD calculator

[**bddcalc.pl**](bddcalc.pl) implements a very simple calculator for
BDDs. It can parse and run the sample files that are contained in this
directory, which are copied from the BuDDy distribution.

The interface predicate is **`run(+File)`**. For example:

    ?- run("c499.cal").
    t2 = id(id0)
    t3 = id(id4)
    t4 = id(t2)xor id(t3)
    ...
    t191 is a tautology.
    t196 is a tautology.
    t201 is a tautology.
    t212 is a tautology.
    t217 is a tautology.
    true.

**Beware**: `bddcalc.pl` uses **internal** predicates of
`library(clpb)`. Changes in these predicates may therefore require
adaptions.
