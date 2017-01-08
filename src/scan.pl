:- module(scan, [scan//2]).

:- use_module(library(dialect/hprolog)).

:- op(600, xfy, <=).
X <= Y :- X =< Y.

scan(BlockInfo, [ V = E | T]) -->
    { E =.. [Op, V1, V2], member(Op, [+,-,*]),
      integer(V1), integer(V2), V is E },
    scan(BlockInfo, T).

scan(BlockInfo, [ V = E | T]) -->
    { E =.. [Op, V1, V2], member(Op, [<,>,'!=','<=','>=']), integer(V1), integer(V2) },
    { E -> V = 1 ; V = 0 },
    scan(BlockInfo, T).

scan(BlockInfo, [V = '=='(_Type, V1, V2) | T]) -->
    % this captures both 1 == 1 and %1 == %1 cases
    { V1 == V2 -> V = 1 ; integer(V1), integer(V2), V1 \== V2, V = 0 },
    scan(BlockInfo, T).

scan(BlockInfo, [ V = phi(_, [(V1, Lab1), (V2, Lab2)]) | T ]) -->
    % one of the labels in PHI is not predecessor
    { Block = BlockInfo.block, Targets = BlockInfo.jumps },
    { \+ memberchk_eq(Lab1 -> Block, Targets) -> V = V2
    ; \+ memberchk_eq(Lab2 -> Block, Targets) -> V = V1 },
    scan(BlockInfo, T).

scan(BlockInfo, [H|T]) --> [H], scan(BlockInfo, T).
scan(_BlockInfo, []) --> [].
