
:- use_module(clpb).

:- initialization(crash).

crash :-
        sat(*([~A+ ~G, ~D+ ~G, 0+B+E+H, ~B+ ~E,
               ~B+ ~H, ~E+ ~H, 0+C+F+I, ~C+ ~F,
               ~C+ ~I, ~F+ ~I, ~A+ ~E, ~A+ ~I, ~B+ ~F,
               ~D+ ~H, ~D+ ~B, ~E+ ~I, ~E+ ~C, ~G+ ~E,
               ~G+ ~C, ~H+ ~F])).
