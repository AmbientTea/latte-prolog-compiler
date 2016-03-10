:- module(expression, [types/4, expect_type/4]).
:- use_module(utils).
:- use_module(environment).

:- op(600, xfy, ++).

%%% PRIMITIVE TYPES %%%%

types(_, int(I), int, int(I) ) :- !.
types(_, str(S), string, str(S) ) :- !.
types(_, true, boolean, true) :- !.
types(_, false, boolean, false) :- !.

%%% VARIABLES %%%

types( Env, var(V), Type, var(V) ) :-
    VarInfo = Env.get_var(V) ->
        Type = VarInfo.type
    ; fail("variable ~w not declared~nin env: ~w", [V, Env]).

%%% OPERATORS %%%

types( Env, E1 + E2, string, NE1 ++ NE2 ) :-
	types(Env, E1, string, NE1), !,
	expect_type(Env, E2, string, NE2).

types( Env, E, int, NE ) :-
	E =.. [Op, E1, E2],
	member(Op, [+, -, *, /, '%']), !,
	expect_type(Env, E1, int, NE1),
	expect_type(Env, E2, int, NE2),
	NE =.. [Op, NE1, NE2].

types(Env, E, boolean, NE) :-
	E =.. [Op, E1, E2],
	member(Op, [<,>,'<=','>=','!=']), !,
	expect_type(Env, E1, int, NE1),
	expect_type(Env, E2, int, NE2),
	NE =.. [Op, NE1, NE2].

types(Env, E, boolean, NE) :-
    E =.. [Op, E1, E2],
    member(Op, ['||', '&&']),
    expect_type(Env, E1, boolean, NE1),
    expect_type(Env, E2, boolean, NE2),
    NE =.. [Op, NE1, NE2].

types(Env, not(E), boolean, not(NE)) :- expect_type(Env, E, boolean, NE).

types(Env, E1 == E2, boolean, '=='(T, NE1, NE2)) :- types(Env, E1, T, NE1), expect_type(Env, E2, T, NE2).

%%% FUNCTIONS %%%

types(Env, app(Fun, Args), Type, app(Fun, NArgs)) :-
    ( FunInfo = Env.functions.get(Fun) -> true ; fail("function ~w does not exist", [Fun])),
    Type = FunInfo.return,
    maplist(types(Env), Args, ArgTypes, NArgs),
    ( ArgTypes = FunInfo.args -> true
    ; fail("function ~w takes arguments of types ~w, but got ~w", [Fun, FunInfo.args, ArgTypes]) ).

% types(Env, Exp, T) :- var(T), fail("cannot type expression: ~w~nin env: ~w", [Exp, Env]).



%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

expect_type(Env, Exp, Type, NExp) :-
    types(Env, Exp, EType, NExp),
    ((Type = EType, !) -> true ; fail("expression ~w has type: ~w, expected: ~w", [Exp, EType, Type])).

