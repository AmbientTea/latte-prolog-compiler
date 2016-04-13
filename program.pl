:- module(program, [correct_program//2]).
:- use_module(utils).
:- use_module(environment).
:- use_module(statement).

correct_program(Program, NProgram) -->
    { emptyenv(Env) }, put_state(Env),
    dgc_map(declare_fun, Program), !,
    correct_functions( Program, NProgram).

declare_fun(topdef(Return, Fun, Args, _)) -->
    get_state(Env),
    { maplist(snd, Args, ArgTypes) },
    put_state( Env.add_fun(Fun, Return, ArgTypes) ).

%%%%%

declare_args([]) --> !.
declare_args([(Id, Type) | T]) -->
    get_state(Env),
    { can_shadow(Env, Id) ->
        NEnv = Env.add_var(Id,Type)
    ; fail("argument ~w declared multiple times", [Id]) },
    put_state(NEnv),
    declare_args(T).

correct_functions([], []) --> !.
correct_functions([H|T], [HH|TT]) --> local(correct_function(H,HH), _), correct_functions(T, TT).

%correct_function(Env, Top, NTop) :- phrase(correct_function(Top, NTop), [Env], _).
correct_function(topdef(Return, Fun, Args, Body),
                 topdef(Return, Fun, Args, NBody)) -->
    do_state(push()),
    declare_args(Args),
    get_state(S1),
    { stmt_monad(Fun, S1, Return, Mon) },
    put_state(Mon),
    !, correct(block(Body), block(NBody)), !,
    get_state(Mon2), !,
    { Mon2.returned = false, Return \= void ->
        fail("control flow reaches function ~w end without return", [Fun])
    ; true }.












