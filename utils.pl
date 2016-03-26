:- module(utils, [fail/1, fail/2, fst/2, snd/2, foldr/4, zip/3, dgc_map//2, separated//3]).


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

zip([], _, []).
zip(_, [], []).
zip([H1|T1], [H2|T2], [(H1,H2)|T]) :- zip(T1, T2, T).

dict_minus(Dict, _{}, Dict) :- !.
dict_minus(Dict, MinDict, NewDict) :-
    _ = MinDict.get(El),
    ( del_dict(El, Dict, _, Dict2), ! ; Dict2 = Dict ),
    del_dict(El, MinDict, _, MinDict2), !,
    dict_minus(Dict2, MinDict2, NewDict).

:- module_transparent separated//3.
separated(_, _, []) --> [].
separated(Sep, Clause, [H | T]) -->
    { ClauseRun =.. [Clause, H] },
    ClauseRun,
    ({T = []} -> [] ; Sep, separated(Sep, Clause, T)).

:- module_transparent dgc_map//2.
dgc_map(_, []) --> [].
dgc_map(Clause, [H|T]) --> { Run =.. [Clause, H] }, Run, dgc_map(Clause, T).

