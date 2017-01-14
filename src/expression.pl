:- module(expression, [types//3, expect_type//3, types//3]).
:- use_module(utils).
:- use_module(environment).
:- use_module(leftval).

:- op(600, xfy, ++).


%%% PRIMITIVE TYPES %%%%
types(int(I), int, int(I)) -->
    { between(-2147483648, 2147483647, I) or_else throw(int_too_big(I)) }.

types(str(S), string, str(S)) --> do_state add_string(S).
types(true, boolean, true) --> !.
types(false, boolean, false) --> !.

%%% VARIABLES %%%
types( LeftVal, Type, NLeftVal ) -->
    leftval(LeftVal, Type, NLeftVal).

%%% AGGREGATE TYPES %%%

types( new(Type), class(Type), new(Type) ) -->
    get_state(Env),
%    { format(user_error, "~w~n", [new : Type]) },
    { Env.classes ? get(Type) or_else throw(bad_class(Type)) }.

types( field(Exp, Field), FieldType, field(Class, NExp, Field) ) -->
    get_state(Env), types(Exp, ExpType, NExp),
    { ExpType = class(Class) or_else
        throw(non_class_field(Exp, ExpType, Field)) },
    
    { ClassInfo = Env.classes.get(Class) or_else
        throw(bad_class(Class)) },
    
    { member(Field - FieldType, ClassInfo.fields) or_else
        throw(bad_field(Class, Field)) }.

%%% OPERATORS %%%

types(neg(Exp), int, int(0) - NExp) -->
    expect_type(Exp, int, NExp).

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
    { member(Fun - FunInfo, FunsInfo) or_else throw(no_function(Fun)) },
    all_type(Args, ArgTypes, NArgs),
    { Type = FunInfo.return },
    { ArgTypes = FunInfo.args or_else throw(bad_args(Fun, FunInfo.args, ArgTypes)) }.

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

all_type([], [], []) --> [].
all_type([H|T], [HT|TT], [NH|NT]) --> types(H, HT, NH), all_type(T, TT, NT).

expect_type(Exp, Type, NExp) -->
    types(Exp, ExpType, NExp),
    { Type = ExpType or_else throw(bad_type(Exp, Type, ExpType)) }.

