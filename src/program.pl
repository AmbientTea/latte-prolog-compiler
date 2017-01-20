:- module(program, [correct_program//2]).
:- use_module(utils).
:- use_module(environment/environment).
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

declare_top(class_def(Class, Fields, Methods)) -->
    get_state(Env),
    { \+ (Env.classes ? get(Class)) or_else throw(dupl_class(Class)) },
    % add variables for real function names
    { maplist(method_type(Class), Methods, MethodTypes) },
    { maplist(method_info(Class), Methods, MethodInfos) },
    do_state add_class(Class, [('$vtable' - ref(struct(MethodTypes))) | Fields], MethodInfos, MethodTypes).

method_info(Class, Id - Type - Args - _Body, Id - MethInfo) :-
    MethInfo = method{
        return: Type,
        args: ArgTypes,
        real_args: [ref(class(Class)) | ArgTypes],
        label: Label },
    maplist(snd, Args, ArgTypes),
    atomic_list_concat(['$', Class, '_', Id], Label).

method_type(Class, _Id - Type - Args - _Block, function(Type, [ref(class(Class)) | ArgTypes])) :-
    maplist(snd, Args, ArgTypes).
%
% FUNCTION DEFINITIONS
%

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

%
% CLASS DEFINITIONS
%

correct_topdef(class_def(Class, Fields, Methods),
               class_def(Class, Fields, Methods1)) -->
    { maplist(fst(-), Fields, FieldNames) },
    { duplicate_in(Field, FieldNames) then throw(dupl_field(Field)) },
    dcg_map(correct_method(Class), Methods, Methods1).


correct_method(Class, Fun - Return - Args - Body,
                      fun_def(Return, NFun, Args1, NBody)) -->
    do_state enter_method(Class, Fun, Return),
    { Args1 = [('$instance', ref(class(Class))) | Args] },
    dcg_map(declare_arg, Args1),
    
    pushed correct(block(Body), block(NBody1)),
    
    get_state(State),
    { State.returned = false ->
        ( Return = void ->
            % void functions need explicit return
            append(NBody1, [return], NBody)
        ; throw( no_return(Fun) ) )
    ; NBody = NBody1 },
    
    { member(Fun - FunInfo, State.classes.Class.methods), NFun = FunInfo.label },
    
    do_state exit_method().
                


