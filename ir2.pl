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

E.add_to(Field, Id, Elem) := E.put(Field, E.Field.put(Id, Elem)).
E.set_block(Block) := E.put(last_block, Block).put(block_known, true).

ir_ask_env(Id, Reg, Env) -->
    ir_empty_env(Env1),
    { Env = Env1.put(ask, Env1.ask.put(Id, Reg)) }.

ir_ask_env(Ask, Env) -->
    ir_empty_env(Env1),
    { Env = Env1.put(ask, Ask) }.

/*
E.reset() := E.put(ask, e{}).put(mod, e{}).put(create,e{}).
E.add_var(Id, Type) := E.put(var_types, E.var_types.put(Id, Type)).
E.get_var(Id) := E.var_types.Id.



E.add_ask(Key, Val) := E :- Val = E.create.get(Key), !.
E.add_ask(Key, Val) := E :- Val = E.mod.get(Key), !.
E.add_ask(Key, Val) := E :- Val = E.ask.get(Key).
E.add_ask(Key, Val) := E.put(ask, E.ask.put(Key, Val)).

E.add_ask(e{}) := E :- !.
E.add_ask(Ask) := E.add_ask(Key, Val).add_ask(Ask2) :-
    select_dict(Key, Ask, Val, Ask2).


E.add_mod_set(e{}) := E :- !.
E.add_mod_set(Mod) := E.add_mod(Key, Val).add_mod_set(Mod2) :-
    select_dict(Key, Mod, Val, Mod2).

E.add_mod(Key, Val) := E2 :-
    E.create ? get(Key) ->
      E2 = E.add_create(Key, Val)
    ; E2 = E.put(mod, E.mod.put(Key, Val)).

E.add_create(Key, Val) := E.put(create, E.create.put(Key,Val)).

*/
%%%%%%%%%%%%%%%
%%% MERGING %%%
%%%%%%%%%%%%%%%

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

/*
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

ir_while_merge_mods(PreEnv, _, e{}, PreEnv) --> [].


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

ir_merge_if_mods(PreEnv, _, e{}, _, e{}, PreEnv) --> [].
ir_merge_if_mods(PreEnv, Label1, e{}, Label2, Mod2, NewEnv) -->
    ir_merge_if_mods(PreEnv, Label2, Mod2, Label1, e{}, NewEnv).
*/
%%%%%%%%%%%%%%%%%%%
%%% ir %%%
%%%%%%%%%%%%%%%%%%%

% EXPRESSIONS_exps(ConstEnv, Env, L, LL, NewEnv) --> dcg_foldl(ir_exp(ConstEnv), Env, L, LL, NewEnv).

ir_exp(_ConstEnv, int(I), I, Env) -->
    ir_empty_env(Env).
ir_exp(_ConstEnv, false, 0, Env) -->
    ir_empty_env(Env).
ir_exp(_ConstEnv, true, 1, Env) -->
    ir_empty_env(Env).

ir_exp(_ConstEnv, var(VarType, Id), Reg, Env) -->
    ir_ask_env(Id, VarType - Reg, Env).


/*
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
*/

ir_exp(ConstEnv, E, V, Env) -->
    { E =.. [Op, E1, E2], member(Op, [+,-,*,/,'%',<,>,'<=','>=']), VV =.. [Op, V1, V2] }, !,
    ir_exp(ConstEnv, E1, V1, Env1),
    ir_exp(ConstEnv, E2, V2, Env2),
    [V = VV],
    { Ask set_is Env1.ask + Env2.ask },
    ir_ask_env(Ask, Env).

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
    
    semicolon_merge(Env1, Env3, Env).

ir_cond(ConstEnv, E1 '||' E2, LabTrue, LabFalse, Env) -->
    ir_exp(ConstEnv, E1, V1, Env1),
    [ if(V1, LabTrue, Second) ],
    
    [ block(Second) ],
    ir_exp(ConstEnv, E2, V2, Env2),
    { Env2.block_known -> Env3 = Env2
    ; Env3 = Env2.set_block(Second), Env2.last_block = Second },
    [ if(V2, LabTrue, LabFalse) ],
    
    semicolon_merge(Env1, Env3, Env).

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
    { Env = ExpEnv.add_to(mod, Id, VarType - V) }.

ir_stmt(_ConstEnv, return, Env) --> [ret], ir_empty_env(Env).
ir_stmt(ConstEnv, return(RetType, Exp), Env) -->
    ir_exp(ConstEnv, Exp, V, Env),
    [ret(RetType, V)].


ir_stmt(_ConstEnv, decl(_Type, []), Env) --> ir_empty_env(Env).

ir_stmt(ConstEnv, decl(Type, [ init(Id, Exp) | T ]), Env) -->
    ir_exp(ConstEnv, Exp, V, ExpEnv),
    ir_stmt(ConstEnv, decl(Type,T), StmtEnv),
    semicolon_merge(ExpEnv.add_to(gen, Id, Type - V), StmtEnv, Env).

/*
ir_stmt(ConstEnv, Env, expstmt(Exp), NewEnv) -->
    ir_exp(ConstEnv, Env, Exp, _, NewEnv).
*/


ir_stmt(ConstEnv, if(If, Then, Else), Env) -->
    ir_exp(ConstEnv, If, V, IfEnv),
    
    [ if(V, ThenBlock, ElseBlock) ],
    
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
    

/*
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
*/


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
    SS = S.put(Id,Reg).


ir_fun(ConstEnv, topdef(Ret, Fun, Args, Body)) -->
    % { format(user_error, "compiling function: ~w : ~w -> ~w~n", [Fun, Args, Ret]) },
    {
        ir_args(Args, _Mod, NArgs),
        phrase(ir_fun_body(ConstEnv, Body, _Env), Code)
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




