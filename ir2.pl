:- module(ir2, [ir_program/3]).

:- use_module(utils).

:- op(600, xfy, ++).
:- op(600, xfy, '&&').
:- op(600, xfy, '||').

% sets need uniform e tag for unification
ir_empty_env(ir2{
    ask: e{},
    mod: e{},
    gen: e{},
    last_block: _Block,
    block_known: false
}) --> [].

E.set_block(Block) := E.put(last_block, Block).put(block_known, true).

ir_ask_env(Id, Reg, Env) -->
    ir_empty_env(Env1),
    { Env = Env1.put(ask/Id, Reg) }.

ir_ask_env(Ask, Env) -->
    ir_empty_env(Env1),
    { Env = Env1.put(ask, Ask) }.

%%%%%%%%%%%%%%%
%%% MERGING %%%
%%%%%%%%%%%%%%%

expression_merge(Env1, Env2, Env) --> 
    ir_empty_env(EmptyEnv),
    {
        Ask set_is Env1.ask + Env2.ask,
        Env = EmptyEnv.put(ask, Ask)
    }.

semicolon_merge( Env1, Env2, Env ) --> {
    Env = ir2{
        ask: Ask,
        mod: Mod,
        gen: Gen,
        last_block: Env2.last_block,
        block_known: Known
    },
    Ask set_is Env1.ask + (Env2.ask ~ Env1.gen ~ Env1.mod),
    Mod set_is Env2.mod + (Env1.mod - Env2.mod) - Env1.gen,
    Gen set_is Env1.gen + Env2.gen,
    ( Env2.block_known -> Known = true
    ; Env1.block_known -> Known = true, Env2.last_block = Env1.last_block
    ;                    Known = false, Env2.last_block = Env1.last_block )
}.

or_merge(Env1, Env2, Env) --> {
    Env = ir2{
        ask: Ask,
        mod: Mod,
        gen: e{},
        last_block: Env2.last_block,
        block_known: true
        },
    Ask set_is Env1.ask + Env2.ask,
    union(Env1.mod.keys(), Env2.mod.keys(), ModKeys)
    },
    
    dcg_map(or_merge_phi(Env1, Env2), ModKeys, NewRegs),
    
    { dict_pairs(Mod, e, NewRegs) }.

or_merge_phi(Env1, Env2, Key, Key - (Type - Reg)) -->
    [ Reg = phi(Type, [(V1, Env1.last_block), (V2, Env2.last_block)]) ],
    {
        (Type - V1 = Env1.mod.get(Key), ! ; Type - V1 = Env1.ask.get(Key)),
        (Type - V2 = Env2.mod.get(Key), ! ; Type - V2 = Env2.ask.get(Key))
    }.

while_merge(StartBlock, WhileEnv, DoEnv, Env) --> {
    Env = ir2{
        ask: Ask,
        mod: Mod,
        gen: e{}
        },
    maplist(fst(-), M, DoEnv.mod.keys()),
    dict_pairs(D, _, M),
    Ask set_is WhileEnv.ask - DoEnv.mod + D + (DoEnv.ask - DoEnv.mod)
    },
    
    dcg_map(while_merge_phi(StartBlock, Ask, DoEnv), DoEnv.mod.keys(), NewRegs),
    
    {
        dict_pairs(Mod, e, NewRegs),
        WhileEnv.ask >:< Mod,
        DoEnv.ask >:< Mod
    }
.

while_merge_phi(StartBlock, WhileAsk, DoEnv, Key, Key - (Type - Reg)) -->
    [ Reg = phi(Type, [(V1, StartBlock), (V2, DoEnv.last_block)]) ],
    {
        Type - V1 = WhileAsk.Key,
        Type - V2 = DoEnv.mod.Key
    }.

%%%%%%%%%%%%%%%%%%%
%%% ir %%%
%%%%%%%%%%%%%%%%%%%

% EXPRESSIONS

ir_exps(_ConstEnv, [], [], Env) --> ir_empty_env(Env).
ir_exps(ConstEnv, [Exp | L], [Reg | LL], Env) -->
    ir_exp(ConstEnv, Exp, Reg, Env1),
    ir_exps(ConstEnv, L, LL, Env2),
    expression_merge(Env1, Env2, Env).
    

ir_exp(_ConstEnv, int(I), I, Env) -->
    ir_empty_env(Env).
ir_exp(_ConstEnv, false, 0, Env) -->
    ir_empty_env(Env).
ir_exp(_ConstEnv, true, 1, Env) -->
    ir_empty_env(Env).

ir_exp(ConstEnv, str(Str), V, Env) -->
    { member(Str - StrLab - Len, ConstEnv.strings) },
    [ V = strcast(Len, StrLab) ],
    ir_empty_env(Env).

ir_exp(_ConstEnv, var(VarType, Id), Reg, Env) -->
    ir_ask_env(Id, VarType - Reg, Env).


ir_exp(ConstEnv, app(Fun, ArgExps), V, Env) -->
    ir_exps(ConstEnv, ArgExps, ArgVals, Env),
    {
        zip(ArgVals, ConstEnv.functions.Fun.args, Args),
        Type = ConstEnv.functions.Fun.return
    },
    ({ Type = void } ->
      [ call(Fun, Args) ]
    ; [ V = call(Type, Fun, Args) ]).


ir_exp(ConstEnv, E1 ++ E2, V, Env) -->
    ir_exp(ConstEnv, app(concat, [E1, E2]), V, Env).

ir_exp(ConstEnv, E, V, Env) -->
    { E =.. [Op, Type, E1, E2], member(Op, ['!=', '==']), VV =.. [Op, Type, V1, V2] }, !,
    ir_exp(ConstEnv, E1, V1, Env1),
    ir_exp(ConstEnv, E2, V2, Env2),
    [V = VV],
    expression_merge(Env1, Env2, Env).

ir_exp(ConstEnv, E, V, Env) -->
    { E =.. [Op, E1, E2], member(Op, [+,-,*,/,'%',<,>,'<=','>=']), VV =.. [Op, V1, V2] }, !,
    ir_exp(ConstEnv, E1, V1, Env1),
    ir_exp(ConstEnv, E2, V2, Env2),
    [V = VV],
    expression_merge(Env1, Env2, Env).

ir_exp(ConstEnv, Exp, V, Env) -->
    % guard for looping
    { member(Exp, [not(_), _ '||' _, _ && _]) }, 
    ir_cond(ConstEnv, Exp, True, False, Env1),
    { Env = Env1.set_block(End) },
    
    [ block(True),
      jump(End) ],
    
    [ block(False),
      jump(End) ],
    
    [ block(End),
      V = phi(boolean, [(1, True), (0, False)]) ].

% boolean expressions

ir_cond(ConstEnv, not(Exp), LabTrue, LabFalse, Env) -->
    ir_cond(ConstEnv, Exp, LabFalse, LabTrue, Env).


ir_cond(ConstEnv, E1 && E2, LabTrue, LabFalse, Env) -->
    ir_exp(ConstEnv, E1, V1, Env1),
    [ if(V1, Second, LabFalse) ],
    
    [ block(Second) ],
    ir_exp(ConstEnv, E2, V2, Env2),
    { Env2.block_known -> Env3 = Env2
    ; Env3 = Env2.set_block(Second), Env2.last_block = Second },
    [ if(V2, LabTrue, LabFalse) ],
    
    expression_merge(Env1, Env3, Env).

ir_cond(ConstEnv, E1 '||' E2, LabTrue, LabFalse, Env) -->
    ir_exp(ConstEnv, E1, V1, Env1),
    [ if(V1, LabTrue, Second) ],
    
    [ block(Second) ],
    ir_exp(ConstEnv, E2, V2, Env2),
    { Env2.block_known -> Env3 = Env2
    ; Env3 = Env2.set_block(Second), Env2.last_block = Second },
    [ if(V2, LabTrue, LabFalse) ],
    
    expression_merge(Env1, Env3, Env).

ir_cond(ConstEnv, Exp, LabTrue, LabFalse, Env) -->
    ir_exp(ConstEnv, Exp, V, Env),
    [ if(V, LabTrue, LabFalse) ].

%%%%%%%%%%%%%%%%%%
%%% STATEMENTS %%%
%%%%%%%%%%%%%%%%%%

ir_stmts(_ConsEnv, [], Env) --> ir_empty_env(Env).
ir_stmts(ConstEnv, [Stmt | Stmts], Env) -->
    ir_stmt(ConstEnv, Stmt, Env1),
    ir_stmts(ConstEnv, Stmts, Env2),
    semicolon_merge(Env1, Env2, Env).

ir_stmt(_ConstEnv, skip, Env) --> ir_empty_env(Env).

ir_stmt(ConstEnv, block(Stmts), Env) -->
    ir_stmts(ConstEnv, Stmts, StmtEnv),
    { Env = StmtEnv.put(gen, e{}) }.

ir_stmt(ConstEnv, (Id : VarType) = Exp, Env) -->
    ir_exp(ConstEnv, Exp, V, ExpEnv),
    { Env = ExpEnv.put(mod/Id, VarType - V) }.

ir_stmt(_ConstEnv, return, Env) --> [ret], ir_empty_env(Env).
ir_stmt(ConstEnv, return(RetType, Exp), Env) -->
    ir_exp(ConstEnv, Exp, V, Env),
    [ret(RetType, V)].


ir_stmt(_ConstEnv, decl(_Type, []), Env) --> ir_empty_env(Env).

ir_stmt(ConstEnv, decl(Type, [ init(Id, Exp) | T ]), Env) -->
    ir_exp(ConstEnv, Exp, V, ExpEnv),
    ir_stmt(ConstEnv, decl(Type,T), StmtEnv),
    semicolon_merge(ExpEnv.put(gen/Id, Type - V), StmtEnv, Env).

ir_stmt(ConstEnv, expstmt(Exp), Env) -->
    ir_exp(ConstEnv, Exp, _, Env).


ir_stmt(ConstEnv, if(If, Then, Else), Env) -->
    ir_cond(ConstEnv, If, ThenBlock, ElseBlock, IfEnv),
    
    [ block(ThenBlock) ],
    ir_stmt(ConstEnv, Then, ThenEnv),
    [ jump(EndBlock) ],
    
    [ block(ElseBlock) ],
    ir_stmt(ConstEnv, Else, ElseEnv),
    [ jump(EndBlock) ],
    
    [ block(EndBlock) ],
    
    or_merge(ThenEnv, ElseEnv, OrEnv),
    semicolon_merge(IfEnv, OrEnv, Env1),
    {
        if_possible (ThenEnv.block_known = false -> ThenEnv.last_block = ThenBlock),
        if_possible (ElseEnv.block_known = false -> ElseEnv.last_block = ElseBlock),
        Env = Env1.put(last_block, EndBlock).put(block_known, true)
    }.
    

ir_stmt(ConstEnv, while(While, Do), Env) -->
    
    [ jump(StartBlock) ],
    [ block(StartBlock) ],
    [ jump(WhileBlock) ],
    
    [ block(WhileBlock) ],
    leave_gap(MergeGap),
    ir_cond(ConstEnv, While, DoBlock, EndBlock, WhileEnv),
    
    [ block(DoBlock) ],
    ir_stmt(ConstEnv, Do, DoEnv),
    [ jump(WhileBlock) ],
    
    [ block(EndBlock) ],
    
    {
        if_possible (WhileEnv.block_known = false -> WhileEnv.last_block = WhileBlock),
        if_possible (DoEnv.block_known = false -> DoEnv.last_block = DoBlock)
    },
    
    fill_gap(MergeGap, while_merge(StartBlock, WhileEnv, DoEnv, Env1)),
    
    { Env = Env1.put(last_block, EndBlock).put(block_known, true) }
. % while


ir_fun_body(ConstEnv, Body, Env) -->
    [ block(_StartBlock) ],
    ir_stmts(ConstEnv, Body, Env),
    % last block can be empty due to returns in bramches.
    [ unreachable ].

%%%%%%%%%%%%%%%
%%% PROGRAM %%%
%%%%%%%%%%%%%%%

ir_args([], e{}, []).
ir_args([(Id,Type) | T], SS, [(Reg,Type) | TT]) :-
    ir_args(T, S, TT),
    SS = S.put(Id,Type - Reg).


ir_fun(ConstEnv, topdef(Ret, Fun, Args, Body)) -->
    % { format(user_error, "compiling function: ~w : ~w -> ~w~n", [Fun, Args, Ret]) },
    {
        ir_args(Args, Mod, NArgs),
        phrase(ir_fun_body(ConstEnv, Body, Env), Code),
        % plug in the arguments
        Mod >:< Env.ask
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




