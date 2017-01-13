:- module(program, [correct_program//2]).
:- use_module(utils).
:- use_module(environment).
:- use_module(statement).

correct_program(Program, NProgram) -->
    { emptyenv(Env) }, put_state(Env),
    dcg_map(declare_top, Program), !,
    dcg_map(correct_topdef, Program, NProgram).

declare_top(fun_def(Return, Fun, Args, _)) -->
    get_state(Env),
    { \+ member(Fun - _, Env.functions) or_else throw(dupl_fun(Fun)) },
    { maplist(snd, Args, ArgTypes) },
    put_state( Env.add_fun(Fun, Return, ArgTypes) ).

declare_top(class_def(_Name, _Fields, _Methods)) -->
    { throw(not_implemented(classes)) }.

%%%%%

declare_arg((Id, void)) -->  { throw(void_arg(Id)) }.
declare_arg((Id, Type)) -->
    can_shadow(Id) -> do_state(add_var(Id,Type)) ; { throw(dupl_arg(Id)) }.

correct_topdef(fun_def(Return, Fun, Args, Body),
               fun_def(Return, Fun, Args, NBody)) -->
    do_state enter_function(Fun, Return),
    dcg_map(declare_arg, Args),
    
    pushed correct(block(Body), block(NBody1)),
    
    get_state(State),
    { State.returned = false ->
        ( Return = void ->
            % void functions need explicit return
            append(NBody1, [return], NBody)
        ; throw( no_return(Fun) ) )
    ; NBody = NBody1 },
    
    do_state exit_function().












