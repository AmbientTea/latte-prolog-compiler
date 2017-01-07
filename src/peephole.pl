:- module(peephole, [peephole//2]).

:- use_module(library(dialect/hprolog)).

:- op(600, xfy, <=).
X <= Y :- X =< Y.

peephole(BlockInfo, [ V = E | T]) -->
    { E =.. [Op, V1, V2], member(Op, [+,-,*]),
      integer(V1), integer(V2), V is E },
    peephole(BlockInfo, T).

peephole(BlockInfo, [ V = E | T]) -->
    { E =.. [Op, V1, V2], member(Op, [<,>,'!=','<=','>=']), integer(V1), integer(V2) },
    { E -> V = 1 ; V = 0 },
    peephole(BlockInfo, T).

peephole(BlockInfo, [V = '=='(_Type, V1, V2) | T]) -->
    { V1 == V2 -> V = 1 ; integer(V1), integer(V2), V1 \== V2, V = 0 },
    peephole(BlockInfo, T).

peephole(BlockInfo, [ V = phi(_, [(V1, Lab1), (V2, Lab2)]) | T ]) -->
    { Block = BlockInfo.block, Targets = BlockInfo.jumps },
    { \+ memberchk_eq(Lab1 -> Block, Targets) -> V = V2
    ; \+ memberchk_eq(Lab2 -> Block, Targets) -> V = V1 },
    peephole(BlockInfo, T).

peephole(BlockInfo, [H|T]) --> [H], peephole(BlockInfo, T).
peephole(_BlockInfo, []) --> [].
