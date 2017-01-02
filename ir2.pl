:- module(ir2, [ir_program/3]).

:- use_module(utils).

:- op(600, xfy, ++).
:- op(600, xfy, '&&').
:- op(600, xfy, '||').

ir_env(ir2{
    ask: _{},
    create: _{},
    mod: _{},
    last_block: ...,
    var_types: _{}
}).

E.reset() := E.put(ask, _{}).put(mod, _{}).put(create,_{}).
E.add_var(Id, Type) := E.put(var_types, E.var_types.put(Id, Type)).
E.get_var(Id) := E.var_types.Id.

E.add_ask(Key, Val) := E.add_ask(_{}.put(Key,Val)).
E.add_ask(_{}) := E :- !.
E.add_ask(Ask) := E2.add_ask(Ask2) :-
    Val = Ask.get(Key),
    del_dict(Key, Ask, Val, Ask2),
    ( Val = E.create.get(Key) -> E2 = E
    ; Val = E.mod.get(Key) -> E2 = E
    ; (E.ask.get(Key) = Val, E2 = E ; E2 = E.put(ask, E.ask.put(Key, Val))) ).

E.add_mod_set(_{}) := E :- !.
E.add_mod_set(Mod) := E.add_mod(Key, Val).add_mod_set(Mod2) :-
    Val = Mod.get(Key),
    del_dict(Key, Mod, Val, Mod2).

E.add_mod(Key, Val) := E2 :-
    E.create ? get(Key) ->
      E2 = E.put(create, E.create.put(Key, Val))
    ; E2 = E.put(mod, E.mod.put(Key, Val)).

E.add_create(Key, Val) := E.put(create, E.create.put(Key,Val)).

%%%%%%%%%%%%%%%
%%% MERGING %%%
%%%%%%%%%%%%%%%

ir_while_merge(PreEnv, PostEnv, NewEnv) -->
    ir_while_merge_mods(PreEnv, PostEnv.last_block, PostEnv.mod, Env1),
    { NewEnv = Env1.add_ask(PostEnv.ask) }
.

ir_while_merge_mods(PreEnv, ModLabel, Mod, NewEnv) -->
    { select_dict(K, Mod, V1, Mod2) },
    { V2 = PreEnv.create.get(K) -> PreEnv2 = PreEnv.add_create(K, V3)
    ; V2 = PreEnv.mod.get(K) -> PreEnv2 = PreEnv.add_mod(K,V3)
    ; V2 = PreEnv.ask.get(K) -> PreEnv2 = PreEnv.add_mod(K,V3)
    ; PreEnv2 = PreEnv.add_ask(K,V2).add_mod(K, V3) },
    
    [ V3 = phi(PreEnv.get_var(K), [(V1, ModLabel), (V2, PreEnv.last_block)]) ],
    ir_while_merge_mods(PreEnv2, ModLabel, Mod2, NewEnv).

ir_while_merge_mods(PreEnv, _, _{}, PreEnv) --> [].


ir_merge_if(PreEnv, PostThenEnv, PostElseEnv, NewEnv) -->
    ir_merge_if_mods(PreEnv.add_ask(PostThenEnv.ask).add_ask(PostElseEnv.ask),
                             PostThenEnv.last_block, PostThenEnv.mod,
                             PostElseEnv.last_block, PostElseEnv.mod, NewEnv).

ir_merge_if_mods(PreEnv, Label1, Mod1, Label2, Mod2, NewEnv) -->
    { select_dict(K, Mod1, V1, NewMod1) },
    {
        ( select_dict(K, Mod2, V2, NewMod2)
        ; V2 = PreEnv.create.get(K), NewMod2 = Mod2
        ; V2 = PreEnv.mod.get(K), NewMod2 = Mod2 ) ->
            PreEnv1 = PreEnv.add_mod(K, V3)
    ;
        PreEnv1 = PreEnv.add_ask(K,V2), NewMod2 = Mod2
    },
    [ V3 = phi(PreEnv.get_var(K), [(V1, Label1), (V2, Label2)]) ],
    ir_merge_if_mods(PreEnv1, Label1, NewMod1, Label2, NewMod2, NewEnv).

ir_merge_if_mods(PreEnv, _, _{}, _, _{}, PreEnv) --> [].
ir_merge_if_mods(PreEnv, Label1, _{}, Label2, Mod2, NewEnv) -->
    ir_merge_if_mods(PreEnv, Label2, Mod2, Label1, _{}, NewEnv).

%%%%%%%%%%%%%%%%%%%
%%% EXPRESSIONS %%%
%%%%%%%%%%%%%%%%%%%

ir_exps(ConstEnv, Env, L, LL, NewEnv) --> dcg_foldl(ir_exp(ConstEnv), Env, L, LL, NewEnv).

ir_exp(_ConstEnv, Env, int(I), I, Env) --> !, [].
ir_exp(_ConstEnv, Env, var(_VarType, Id), Reg, Env.add_ask(Id, Reg)) --> !, [].
ir_exp(_ConstEnv, Env, false, 0, Env) --> !, [].
ir_exp(_ConstEnv, Env, true, 1, Env) --> !, [].
ir_exp(ConstEnv, Env, str(Str), V, Env) -->
    { member(Str - StrLab - Len, ConstEnv.strings) },
    [ V = strcast(Len, StrLab) ].

ir_exp(ConstEnv, Env, app(Fun, ArgExps), V, NewEnv) -->
    ir_exps(ConstEnv, Env, ArgExps, ArgVals, NewEnv),
    { zip(ArgVals, ConstEnv.functions.Fun.args, Args) },
    {Type = ConstEnv.functions.Fun.return},
    ({ Type = void } ->
      [ call(Fun, Args) ]
    ; [ V = call(Type, Fun, Args) ]).

ir_exp(ConstEnv, Env, E1 ++ E2, V, NewEnv) --> ir_exp(ConstEnv, Env, app(concat, [E1, E2]), V, NewEnv).

ir_exp(ConstEnv, Env, E, V, NewEnv) -->
    { E =.. [Op, Type, E1, E2], member(Op, ['!=', '==']), VV =.. [Op, Type, V1, V2] }, !,
    ir_exp(ConstEnv, Env, E1, V1, Env1),
    ir_exp(ConstEnv, Env1, E2, V2, NewEnv),
    [V = VV], !.

ir_exp(ConstEnv, Env, E, V, NewEnv) -->
    { E =.. [Op, E1, E2], member(Op, [+,-,*,/,'%',<,>,'<=','>=']), VV =.. [Op, V1, V2] }, !,
    ir_exp(ConstEnv, Env, E1, V1, Env1),
    ir_exp(ConstEnv, Env1, E2, V2, NewEnv),
    [V = VV].

ir_exp(ConstEnv, Env, Exp, V, NewEnv) -->
    ir_cond(ConstEnv, Env, Exp, True, False, NewEnv),
    
    [ block(True),
      jump(End) ],
    
    [ block(False),
      jump(End) ],
    
    [ block(End), V = phi(boolean, [(1, True), (0, False)]) ].
    
ir_exp(_,_, E, _, _) --> { writeln(fail:E), halt }.

ir_cond(ConstEnv, Env, not(Exp), LabTrue, LabFalse, NewEnv) -->
    ir_cond(ConstEnv, Env, Exp, LabFalse, LabTrue, NewEnv).

ir_cond(ConstEnv, Env, E1 && E2, LabTrue, LabFalse, NewEnv) -->
    ir_exp(ConstEnv, Env, E1, V1, Env1),
    [ if(V1, Second, LabFalse) ],
    
    [ block(Second) ],
    ir_exp(ConstEnv, Env1, E2, V2, NewEnv),
    [ if(V2, LabTrue, LabFalse) ].

ir_cond(ConstEnv, Env, E1 '||' E2, LabTrue, LabFalse, NewEnv) -->
    ir_exp(ConstEnv, Env, E1, V1, Env1),
    [ if(V1, LabTrue, Second) ],
    
    [ block(Second) ],
    ir_exp(ConstEnv, Env1, E2, V2, NewEnv),
    [ if(V2, LabTrue, LabFalse) ].

ir_cond(ConstEnv, Env, Exp, LabTrue, LabFalse, NewEnv) -->
    ir_exp(ConstEnv, Env, Exp, V, NewEnv),
    [ if(V, LabTrue, LabFalse) ].
%%%%%%%%%%%%%%%%%%
%%% STATEMENTS %%%
%%%%%%%%%%%%%%%%%%

ir_stmts(ConstEnv, Env, Stmts, NewEnv) --> dcg_foldl(ir_stmt(ConstEnv), Env, Stmts, NewEnv).

ir_stmt(_ConstEnv, Env, skip, Env) --> [].

ir_stmt(ConstEnv, InEnv, block(Stmts), NewEnv) -->
    { TempEnv = InEnv.reset() },
    ir_stmts(ConstEnv, TempEnv, Stmts, OutEnv),
    { NewEnv = InEnv.add_ask(OutEnv.ask).add_mod_set(OutEnv.mod).put(last_block, OutEnv.last_block) }.

ir_stmt(ConstEnv, InEnv, (Id : _VarType) = Exp, OutEnv) -->
    ir_exp(ConstEnv, InEnv, Exp, V, ExpEnv),
    { OutEnv = ExpEnv.add_mod(Id, V) }.

ir_stmt(_ConstEnv, Env, return, Env) --> [ret].
ir_stmt(ConstEnv, Env, return(RetType, Exp), NewEnv) -->
    ir_exp(ConstEnv, Env, Exp, V, NewEnv),
    [ret(RetType, V)].

ir_stmt(_ConstEnv, Env, decl(_, []), Env) --> [].

ir_stmt(ConstEnv, Env, decl(Type, [ init(Id, Exp) | T ]), NewEnv) -->
    ir_exp(ConstEnv, Env, Exp, V, ExpEnv),
    ir_stmt(ConstEnv, ExpEnv.add_var(Id, Type).add_create(Id, V), decl(Type,T), NewEnv).


ir_stmt(ConstEnv, Env, expstmt(Exp), NewEnv) -->
    ir_exp(ConstEnv, Env, Exp, _, NewEnv).

ir_stmt(ConstEnv, Env, if(If, Then, Else), NewEnv) -->
    ir_exp(ConstEnv, Env, If, V, CondEnv),
    [ if(V, ThenBlock, ElseBlock) ],
    
    { EmptyEnv = CondEnv.reset() },
    
    ir_block(ConstEnv, EmptyEnv, ThenBlock, ThenEnv),
    ir_stmt(ConstEnv, ThenEnv, Then, PostThenEnv),
    [ jump(EndBlock) ],
    
    ir_block(ConstEnv, EmptyEnv, ElseBlock, ElseEnv),
    ir_stmt(ConstEnv, ElseEnv, Else, PostElseEnv),
    [ jump(EndBlock) ],
    
    ir_block(ConstEnv, Env, EndBlock, PostEnv),
    ir_merge_if(PostEnv.add_ask(CondEnv.ask), PostThenEnv, PostElseEnv, NewEnv).
    

ir_stmt(ConstEnv, Env, while(While, Do), NewEnv) -->
    { EmptyEnv = Env.reset() },
    
    [ jump(CondBlock) ],
    
    ir_block(ConstEnv, EmptyEnv, DoBlock, DoEnv),
    ir_stmt(ConstEnv, DoEnv, Do, PostDoEnv),
    [ jump(CondBlock) ],
    
    ir_block(ConstEnv, EmptyEnv, CondBlock, _),
    ir_while_merge(Env, PostDoEnv, MergeEnv),
    ir_exp(ConstEnv, MergeEnv, While, V, CondEnv),
    [ if(V, DoBlock, EndBlock) ],
    
    ir_block(ConstEnv, CondEnv, EndBlock, NewEnv)
. % while

% ir_stmt(S) --> { writeln(S), fail }.

ir_block(_ConstEnv, Env, Label, Env.put(last_block, Label)) --> [ block(Label) ].



ir_fun_body(ConstEnv, FunEnv, Body) -->
    [ block(StartBlock) ],
    ir_stmts(ConstEnv, FunEnv.put(last_block,StartBlock), Body, _),
    % last block can be empty due to returns in bramches.
    [ unreachable ].

%%%%%%%%%%%%%%%
%%% PROGRAM %%%
%%%%%%%%%%%%%%%

ir_args([], _{}, []).
ir_args([(Id,Type) | T], SS, [(Reg,Type) | TT]) :-
    ir_args(T, S, TT),
    SS = S.put(Id,Reg).


ir_fun(ConstEnv, topdef(Ret, Fun, Args, Body)) -->
    % { format(user_error, "compiling function: ~w : ~w -> ~w~n", [Fun, Args, Ret]) },
    {
        ir_env(Env),
        ir_args(Args, Mod, NArgs),
        FunEnv = Env.add_mod_set(Mod),
        phrase(ir_fun_body(ConstEnv, FunEnv, Body), Code)
    },
    [ function(Ret, Fun, NArgs, Code) ].

ir_fun_decl(Fun - FunInfo) -->
        { FunInfo.extern = false }
    ;
        [ decl(Fun, FunInfo.return, FunInfo.args) ].

ir_str_decl(Str1 - Lab - Len) -->
    { string_concat(Str1, "\\00", Str) },
    [ string(Str, Lab, Len) ].



program(Env, Program) -->
    dcg_map(ir_fun_decl, Env.functions),
    dcg_map(ir_str_decl, Env.strings),
    dcg_map(ir_fun(Env), Program).


ir_program(Env, Program, IR) :-
    phrase(program(Env, Program), IR), !
.




