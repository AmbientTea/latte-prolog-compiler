:- module(expression, [types//3, expect_type//3, types//3]).
:- use_module(utils).
:- use_module(environment).

:- op(600, xfy, ++).


%%% PRIMITIVE TYPES %%%%
types(int(I), int, int(I)) --> !.
types(str(S), string, str(S)) --> !.
types(true, boolean, true) --> !.
types(false, boolean, false) --> !.

%%% VARIABLES %%%
types( var(V), Type, var(V) ) -->
    ask_state(get_var(V), VarInfo) ->
        { Type = VarInfo.type }
    ; { fail("variable ~w not declared", [V]) }.

%%% OPERATORS %%%

types( E1 + E2, string, NE1 ++ NE2 ) -->
	types(E1, string, NE1), !,
	expect_type(E2, string, NE2).

types( E, int, NE ) -->
	{ E =.. [Op, E1, E2] },
	{ member(Op, [+, -, *, /, '%']) }, !,
	expect_type(E1, int, NE1),
	expect_type(E2, int, NE2),
	{ NE =.. [Op, NE1, NE2] }.

types(E, boolean, NE) -->
	{ E =.. [Op, E1, E2] },
	{ member(Op, [<,>,'<=','>=']) }, !,
	expect_type(E1, int, NE1),
	expect_type(E2, int, NE2),
	{ NE =.. [Op, NE1, NE2] }.

types(E, boolean, NE) -->
    { E =.. [Op, E1, E2] },
    { member(Op, ['||', '&&']) },
    expect_type(E1, boolean, NE1),
    expect_type(E2, boolean, NE2),
    { NE =.. [Op, NE1, NE2] }.

types(not(E), boolean, not(NE)) --> expect_type(E, boolean, NE).

types(E1 == E2, boolean, '=='(T, NE1, NE2)) --> types(E1, T, NE1), expect_type(E2, T, NE2).
types('!='(E1, E2), boolean, '!='(T, NE1, NE2)) --> types(E1, T, NE1), expect_type(E2, T, NE2).

%%% FUNCTIONS %%%

types(app(Fun, Args), Type, app(Fun, NArgs)) -->
    ask_state(functions, FunsInfo),
    { FunInfo = FunsInfo.get(Fun), ! ; fail("function ~w does not exist", [Fun]) },
    all_type(Args, ArgTypes, NArgs),
    { Type = FunInfo.return },
    { ArgTypes = FunInfo.args, !
    ; fail("function ~w takes arguments of types ~w, but got ~w", [Fun, FunInfo.args, ArgTypes]) }.

% types(Env, Exp, T) :- var(T), fail("cannot type expression: ~w~nin env: ~w", [Exp, Env]).

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

all_type([], [], []) --> [].
all_type([H|T], [HT|TT], [NH|NT]) --> types(H, HT, NH), all_type(T, TT, NT).

expect_type(Exp, Type, NExp) -->
    types(Exp, EType, NExp),
    { Type = EType -> true
    ; fail("expression ~w has type: ~w, expected: ~w", [Exp, EType, Type]) }.

