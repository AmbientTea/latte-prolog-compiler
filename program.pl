:- module(program, [correct_program//2]).
:- use_module(utils).
:- use_module(environment).
:- use_module(statement).

correct_program(Program, NProgram) -->
    { emptyenv(Env) }, put_state(Env),
    dcg_map(declare_fun, Program), !,
    correct_functions( Program, NProgram).

declare_fun(topdef(Return, Fun, Args, _)) -->
    get_state(Env),
    { maplist(snd, Args, ArgTypes) },
    put_state( Env.add_fun(Fun, Return, ArgTypes) ).

%%%%%

declare_args([]) --> !.
declare_args([(Id, Type) | T]) -->  
    ( can_shadow(Id) -> 
        do_state(add_var(Id,Type))
    ; { fail("argument ~w declared multiple times", [Id]) }),
    declare_args(T).

correct_functions([], []) --> !.
correct_functions([H|T], [HH|TT]) --> pushed correct_function(H,HH), !, correct_functions(T, TT).

correct_function(topdef(Return, Fun, Args, Body),
                 topdef(Return, Fun, Args, NBody)) -->
    do_state put(returned, false),
    declare_args(Args),
    do_state put(return_type, Return),
    do_state put(function_name, Fun),
    
    pushed correct(block(Body), block(NBody1)),
    
    get_state(State),
    { State.returned = false ->
        ( Return = void ->
            % void functions need explicit return
            append(NBody1, [return], NBody)
        ; fail("control flow reaches function ~w end without return", [Fun]) )
    ; NBody = NBody1 }.












