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

declare_top(class_def(Class, Super, Fields, Methods)) -->
    get_state(Env),
    { \+ (Env.classes ? get(Class)) or_else throw(dupl_class(Class)) },
    { (Super == '$none' ; Env.classes ? get(Super)) or_else throw(bad_superclass(Super, Class)) },
    % add variables for real function names
    { ClassInfo1 = class{
          fields: [('$vtable' - ref(class(VTLabel))) | Fields],
          methods: MethodInfos,
          vtable_label: VTable,
          vtable_type: struct(MethodTypes),
          vtable_type_label: class(VTLabel) },
      atomic_list_concat(['_', Class, '_vtable'], VTable),
      atomic_list_concat(['_', Class, '_vtable_T'], VTLabel),

      % own method declarations
      maplist(method_info(Class), Methods, MethodInfos1),
      
      ( Super == '$none' ->
          ClassInfo = ClassInfo1,
          MethodInfos = MethodInfos1
      ;
          SupInfo = Env.classes.Super,
          ClassInfo = ClassInfo1.put(superclass, Super),
          inherit__methods(SupInfo.methods, MethodInfos1, MethodInfos) ),
      maplist(method_type, MethodInfos, MethodTypes)
      
    },

    do_state put(classes/Class, ClassInfo).

method_info(Class, Id - Type - Args - _Body, Id - MethInfo) :-
    MethInfo = method{
        return: Type,
        args: ArgTypes,
        real_args: [ref(class(Class)) | ArgTypes],
        label: Label },
    maplist(snd, Args, ArgTypes),
    atomic_list_concat(['_', Class, '__', Id], Label).

method_type(_Meth - MethInfo, function(MethInfo.return, MethInfo.real_args)).

% merges superclass and subclass method lists, substituting overrides
% and preserving order
inherit__methods([], Infos, Infos).
inherit__methods([Meth - SInfo | STail], Infos, [Meth - Info | Tail]) :-
    ( select(Meth - Info, Infos, NInfos) or_else Info = SInfo, NInfos = Infos),
    inherit__methods(STail, NInfos, Tail).
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

correct_topdef(class_def(Class, Super, Fields, Methods),
               class_def(Class, Super, Fields, Methods1)) -->
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


