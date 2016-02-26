:- module(expression, [types/3, expect_type/3]).
:- use_module(utils).
:- use_module(environment).

%%% PRIMITIVE TYPES %%%%

types(_, int(_), int ) :- !.
types(_, str(_), string ) :- !.
types(_, true, boolean) :- !.
types(_, false, boolean) :- !.

%%% VARIABLES %%%

types( Env, var(V), Type ) :-
    VarInfo = Env.get_var(V) ->
        Type = VarInfo.type
    ; fail("variable ~w not declared~nin env: ~w", [V, Env]).

%%% OPERATORS %%%

types( Env, E1 + E2, string ) :-
	types(Env, E1, string), !,
	expect_type(Env, E2, string).

types( Env, E, int ) :-
	E =.. [Op, E1, E2],
	member(Op, [+, -, *, /, '%']), !,
	expect_type(Env, E1, int),
	expect_type(Env, E2, int), !.

types(Env, E, boolean) :-
	E =.. [Op, E1, E2],
	member(Op, [<,>,'<=','>=','!=']), !,
	expect_type(Env, E1, int),
	expect_type(Env, E2, int), !.

types(Env, E, boolean) :-
    E =.. [Op, E1, E2],
    member(Op, ['||', '&&']),
    expect_type(Env, E1, boolean),
    expect_type(Env, E2, boolean).

types(Env, not(E), boolean) :- expect_type(Env, E, boolean).

types(Env, E1 == E2, boolean) :- types(Env, E1, T), expect_type(Env, E2, T).

%%% FUNCTIONS %%%

types(Env, app(Fun, Args), Type) :-
    ( FunInfo = Env.functions.get(Fun) -> true ; fail("function ~w does not exist", [Fun])),
    Type = FunInfo.return,
    maplist(types(Env), Args, ArgTypes),
    ( ArgTypes = FunInfo.args -> true
    ; fail("function ~w takes arguments of types ~w, but got ~w", [Fun, FunInfo.args, ArgTypes]) ).

% types(Env, Exp, T) :- var(T), fail("cannot type expression: ~w~nin env: ~w", [Exp, Env]).



%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

expect_type(Env, Exp, Type) :-
    types(Env, Exp, EType),
    ((Type = EType, !) -> true ; fail("expression ~w has type: ~w, expected: ~w", [Exp, EType, Type])).

