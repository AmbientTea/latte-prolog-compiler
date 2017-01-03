:- module('$utils', [
    op(700, xfy, :==),
    op(600, xfy, ?),
    op(500, yfx, ~),
    op(700, xfx, set_is), set_is/2,
    fail/1, fail/2,
    fst/2, snd/2,
    foldr/4,
    zip/3,
    dcg_map//2, dcg_map//3,
    separated//3,
    dcg_foldl//4, dcg_foldl//5, '?'/2,
    get_state//1, put_state//1,
    do_state//1, op(600, fx, do_state),
    ask_state//2,
    local//2, local//1, op(600, fx, local),
    subtract_eq/3,
    select_dict/4,
    keys//0
]).

% needed for memberchk_eq/2
:- use_module(library(dialect/hprolog)).

%%%%%%%%%%%%%%%%%%%%
% Minor predicates %
%%%%%%%%%%%%%%%%%%%%

fst((A,_), A).
fst(X, A) :- X =.. [_, A | _].

snd((_, B), B).
snd(X, B) :- X =.. [_, _, B | _].

%%%%%%%%%%%%%%%%%%%%%%
% DGC State Handling %
%%%%%%%%%%%%%%%%%%%%%%

get_state(S), [S] --> [S].
put_state(S), [S] --> [_] ; [].

do_state F, [NS] --> [S], { NS = S.F }.

ask_state(A, V) --> get_state(S), { V = S.A }.

:- module_transparent 'local'//1, 'local'//2.
local(Instr) --> get_state(S), Instr, put_state(S).
local(Instr, St) --> get_state(S), Instr, get_state(St), put_state(S).

fail(S, A) :- string_concat(S,"~n",SS), format(user_error, SS, A), fail.
fail(S) :- fail(S, []).

%%%%%%%%%%%%%%%%%%%
% List predicates %
%%%%%%%%%%%%%%%%%%%

:- meta_predicate foldr(3, ?, ?, ?).
foldr(_, Zero, [], Zero ).
foldr(Fun, Zero, [H | Args], Ret) :-
    call(Fun, Zero, H, NZero),
    foldr(Fun, NZero, Args, Ret).

zip([], _, []).
zip(_, [], []).
zip([H1|T1], [H2|T2], [(H1,H2)|T]) :- zip(T1, T2, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG higher-level predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% separated(+Separator, +Closure, +List)
% calls Closure for each element in the List, calling Separator between them
:- module_transparent separated//3.
separated(_, _, []) --> [].
separated(Sep, Clause, [H | T]) -->
    { ClauseRun =.. [Clause, H] },
    ClauseRun,
    ({T = []} -> [] ; Sep, separated(Sep, Clause, T)).

% dcg_map(+Closure, ?List)
% succeeds if Closure succeeds for each element in List
:- module_transparent dcg_map//2, dcg_map//3.
dcg_map(_, []) --> [].
dcg_map(Clause, [H|T]) -->
    { Clause =.. [Op | Args], !, append(Args, [H], Args2), Run =.. [Op | Args2] }, Run, dcg_map(Clause, T).
dcg_map(Clause, [H|T]) --> { Run =.. [Clause, H] }, Run, dcg_map(Clause, T).

% dcg_map(+Closure, ?List1, ?List2)
% succeeds if Closure succeeds for each pair of coresponding elements in List1, List2
dcg_map(_, [], []) --> [].
dcg_map(Clause, [H|T], [HH|TT]) -->
    { Clause =.. [Op | Args], !, append(Args, [H, HH], Args2), Run =.. [Op | Args2] }, Run, dcg_map(Clause, T, TT).
dcg_map(Clause, [H|T], [HH|TT]) --> { Run =.. [Clause, H, HH] }, Run, dcg_map(Clause, T, TT).

% dcg_folfl(+Clause, +LeftValue, +List, ?RightValue)
% DGC analogon of functional fold left
:- module_transparent dcg_foldl//4, dcg_foldl//5.
dcg_foldl(_, V, [], V) --> [].
dcg_foldl(Clause, V1, [H|T], V2) -->
    { Clause =.. DstrClause,
      append(DstrClause, [V1, H, V3], NewClause),
      Run =.. NewClause },
    Run,
    dcg_foldl(Clause, V3, T, V2).

% dcg_folfl(+Clause, +LeftValue, +List, ?RightValue)
% DGC analogon of functional fold left, iterating over two lists
dcg_foldl(_, V, [], [], V) --> [].
dcg_foldl(Clause, V1, [H1|T1], [H2|T2], V2) -->
    { Clause =.. DstrClause,
      append(DstrClause, [V1, H1, H2, V3], NewClause),
      Run =.. NewClause },
    Run,
    dcg_foldl(Clause, V3, T1, T2, V2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dict handling predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% subtraction of sets over keys without unification of values
dict_minus(Dict, _{}, Dict) :- !.
dict_minus(Dict, MinDict, NewDict) :-
    select_dict(Key, MinDict, _, MinDict2), !,
    ( del_dict(Key, Dict, _, Dict2), ! ; Dict2 = Dict ),
    dict_minus(Dict2, MinDict2, NewDict).

% behaves like del_dict but allows for backtracking over keys
select_dict(Key, DictIn, Value, DictOut) :-
    Value = DictIn.get(Key), del_dict(Key, DictIn, Value, DictOut).

:- multifile user:term_expansion/2.
user:term_expansion(Head :== Exp, Head := V :- V is Exp).
user:term_expansion((Head :== Exp :- Body0), (Head := V :- Body0, V is Exp)).

?(M, F) :- _ = M.F.

:- module_transparent keys//0.
D.keys() := Keys :-
    dict_pairs(D, _, Pairs),
    maplist(fst, Pairs, Keys).

% works like subtract/3 but uses memberchk_eq
subtract_eq([], _, []).
subtract_eq([Elem | Set], Sub, Result) :-
    memberchk_eq(Elem, Sub), !,
    subtract_eq(Set, Sub, Result).
subtract_eq([Elem | Set], Sub, [Elem | Result]) :-
    subtract_eq(Set, Sub, Result).

% set operation analogon for is/2. without one complex set operations get clunky
% union
(Val set_is (Exp1 + Exp2)) :-
    V1 set_is Exp1,
    V2 set_is Exp2,
    V1 >:< V2,
    Val = V1.put(V2).
% non-unifying subtraction
(Val set_is (Exp1 - Exp2)) :-
    V1 set_is Exp1,
    V2 set_is Exp2,
    dict_minus(V1, V2, Val).
% unifying subtraction
(Val set_is (Exp1 ~ Exp2)) :-
    V1 set_is Exp1,
    V2 set_is Exp2,
    V1 >:< V2,
    dict_minus(V1, V2, Val).
Val set_is Val :- is_dict(Val).
