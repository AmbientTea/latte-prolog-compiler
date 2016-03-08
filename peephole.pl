:- module(peephole, [peephole//1]).

peephole([]) --> [].


peephole([ V = E | T]) -->
    { E =.. [Op, V1, V2], member(Op, [+,-,*]), integer(V1), integer(V2), V is E }, peephole(T).
% TODO: other operators




peephole([ V = E | T]) -->
    { E =.. [Op, V1, V2], member(Op, [<,>,==,'!=','<=','>=']), integer(V1), integer(V2) },
    { E -> V = 1 ; V = 0 },
    peephole(T).

% peephole([block(B1), jmp(B2) | T]) --> { B1 = B2 }, peephole(T).

peephole([block(B1), block(B2) | T]) --> { B1 = B2 }, [ block(B1) ], peephole(T).

peephole([if(1, Bl, _) | T]) --> [jmp(Bl)], peephole(T).
peephole([if(0, _, Bl) | T]) --> [jmp(Bl)], peephole(T).



peephole([H|T]) --> [H], peephole(T).
