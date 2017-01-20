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
types(null, ref(_), null) --> [].

types(cast(Type, null), Type, null) -->
    { Type = ref(_) or_else throw(bad_cast(null, ref(...), Type)) }.

types(cast(Type, Exp), Type, NCast) -->
    types(Exp, ExpType, NExp),
    { ExpType == Type -> NCast = NExp
    % NCast = cast(Type, ExpType, NExp)
    ; throw(bad_cast(Exp, ExpType, Type)) }.

types(field(Exp, length), int, arr_length(Type, NExp)) -->
    types(Exp, ref(array(Type)), NExp), !.

%%% VARIABLES %%%
types( LeftVal, Type, NLeftVal ) -->
    leftval(LeftVal, Type, NLeftVal).

%%% AGGREGATE TYPES %%%
types( new(Type), ref(class(Type)), new(Type) ) -->
    get_state(Env),
%    { format(user_error, "~w~n", [new : Type]) },
    { Env.classes ? get(Type) or_else throw(bad_class(Type)) }.

types( new_arr(Type, Exp), ref(array(Type)), new_arr(Type, NExp) ) -->
    types(Exp, ExpType, NExp),
    { ExpType == int or_else throw(bad_new_len(Exp, ExpType)) }.

%%% OPERATORS %%%
types(neg(Exp), int, int(0) - NExp) -->
    expect_type(Exp, int, NExp).

types( E1 + E2, string, NE1 ++ NE2 ) -->
	types(E1, Type, NE1), { Type == string }, !,
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

types(method(Exp, Meth, Args), Type, method(Class, NExp, Meth, NArgs)) -->
    get_state(Env),
    types(Exp, EType, NExp),
    
    { EType = ref(class(Class)) or_else throw(non_class_method_call(Exp, Meth, EType)) },
    
    { member(Meth - MethInfo, Env.classes.Class.methods) or_else throw(bad_method_call(Class, Meth)) },
    
    all_type(Args, ArgTypes, NArgs),
    
    { ArgTypes = MethInfo.args or_else throw(bad_args(Class:Meth, MethInfo.args, ArgTypes)) },
    { Type = MethInfo.return }.

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

all_type([], [], []) --> [].
all_type([H|T], [HT|TT], [NH|NT]) --> types(H, HT, NH), all_type(T, TT, NT).

expect_type(Exp, Type, NExp) -->
    types(Exp, ExpType, NExp),
    { Type = ExpType or_else throw(bad_type(Exp, Type, ExpType)) }.

