:- module(program, [correct_program/3]).
:- use_module(utils).
:- use_module(environment).
:- use_module(statement).

arg_types([], []).
arg_types([(_, Tp) | T], [Tp | TT]) :- arg_types(T, TT).

correct_program(Program, Env, NProgram) :- 
    emptyenv(EEnv),
    foldr(declare_fun, EEnv, Program, Env),
    maplist(correct_function(Env), Program, NProgram).

declare_fun(Env, topdef(Return, Fun, Args, _), NEnv) :-
    arg_types(Args, ArgTypes),
    NEnv = Env.add_fun(Fun, Return, ArgTypes).

%%%%%

declare_args([]) --> !.
declare_args([(Id, Type) | T]) -->
    get_state(Env),
    { can_shadow(Env, Id) ->
        NEnv = Env.add_var(Id,Type)
    ; fail("argument ~w declared multiple times", [Id]) },
    put_state(NEnv),
    declare_args(T).


correct_function(Env, Top, NTop) :- phrase(correct_function(Top, NTop), [Env], _).
correct_function(topdef(Return, Fun, Args, Body),
                 topdef(Return, Fun, Args, NBody)) -->
    do_state(push()),
    declare_args(Args),
    get_state(S1),
    { stmt_monad(Fun, S1, Return, Mon) },
    put_state(Mon),
    !, correct(block(Body), block(NBody)), !,
    get_state(Mon2),
    { Mon2.returned = false, Return \= void ->
        fail("control flow reaches function ~w end without return", [Fun])
    ; true }.












