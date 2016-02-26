:- module(utils, [fail/1, fail/2, fst/2, snd/2, foldr/4]).


fail(S, A) :- string_concat(S,"~n",SS), format(SS, A), fail.
fail(S) :- fail(S, []).

fst((A,_), A).
fst(X, A) :- X =.. [_, A | _].

snd((_, B), B).
snd(X, B) :- X =.. [_, _, B | _].

:- meta_predicate foldr(3, ?, ?, ?).
foldr(_, Zero, [], Zero ).
foldr(Fun, Zero, [H | Args], Ret) :-
    call(Fun, Zero, H, NZero),
    foldr(Fun, NZero, Args, Ret).
