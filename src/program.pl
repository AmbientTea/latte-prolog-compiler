:- module(program, [correct_program//2]).
:- use_module(utils).
:- use_module(environment).
:- use_module(statement).

correct_program(Program, NProgram) -->
    { emptyenv(Env) }, put_state(Env),
    dcg_map(declare_top, Program), !,
    correct_functions( Program, NProgram).

declare_top(fun_def(Return, Fun, Args, _)) -->
    get_state(Env),
    { \+ member(Fun - _, Env.functions) or_else throw(dupl_fun(Fun)) },
    { maplist(snd, Args, ArgTypes) },
    put_state( Env.add_fun(Fun, Return, ArgTypes) ).

%%%%%

declare_args([]) --> !.

declare_args([(Id, void) | _]) -->  { throw(void_arg(Id)) }.
declare_args([(Id, Type) | T]) -->  
    ( can_shadow(Id) -> 
        do_state(add_var(Id,Type))
    ; { throw(dupl_arg(Id)) }),
    declare_args(T).

correct_functions([], []) --> !.
correct_functions([H|T], [HH|TT]) --> pushed correct_function(H,HH), !, correct_functions(T, TT).

correct_function(fun_def(Return, Fun, Args, Body),
                 fun_def(Return, Fun, Args, NBody)) -->
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
        ; throw( no_return(Fun) ) )
    ; NBody = NBody1 }.












