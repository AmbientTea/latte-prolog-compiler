:- module(ir2, [ir_program/3]).

:- use_module(utils).

:- op(600, xfy, ++).
:- op(600, xfy, '&&').
:- op(600, xfy, '||').


ir_env(Type, Funs, Strings, ir2{
    ask: _{},
    create: _{},
    mod: _{},
    last_block: ...,
    return_type: Type,
    var_types: _{},
    funs: Funs,
    strings: Strings
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
    { V1 = Mod.get(K), del_dict(K, Mod, V1, Mod2) },
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
    { V1 = Mod1.get(K), del_dict(K, Mod1, V1, NewMod1) },
    {
        ( V2 = Mod2.get(K), del_dict(K, Mod2, V2, NewMod2)
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

ir_exps(Env, L, LL, NewEnv) --> dcg_foldl(ir_exp, Env, L, LL, NewEnv).

ir_exp(Env, int(I), I, Env) --> !, [].
ir_exp(Env, var(Id), Reg, Env.add_ask(Id, Reg)) --> !, [].
ir_exp(Env, false, 0, Env) --> !, [].
ir_exp(Env, true, 1, Env) --> !, [].
ir_exp(Env, str(Str), V, Env) -->
    { member(Str - StrLab - Len, Env.strings) },
    [ V = strcast(Len, StrLab) ].

ir_exp(Env, app(Fun, ArgExps), V, NewEnv) -->
    ir_exps(Env, ArgExps, ArgVals, NewEnv),
    { zip(ArgVals, Env.funs.Fun.args, Args) },
    {Type = Env.funs.Fun.return},
    ({ Type = void } ->
      [ call(Fun, Args) ]
    ; [ V = call(Type, Fun, Args) ]).

ir_exp(Env, E1 ++ E2, V, NewEnv) --> ir_exp(Env, app(concat, [E1, E2]), V, NewEnv).

ir_exp(Env, E, V, NewEnv) -->
    { E =.. [Op, Type, E1, E2], member(Op, ['!=', '==']), VV =.. [Op, Type, V1, V2] }, !,
    ir_exp(Env, E1, V1, Env1),
    ir_exp(Env1, E2, V2, NewEnv),
    [V = VV], !.

ir_exp(Env, E, V, NewEnv) -->
    { E =.. [Op, E1, E2], member(Op, [+,-,*,/,'%',<,>,'<=','>=']), VV =.. [Op, V1, V2] }, !,
    ir_exp(Env, E1, V1, Env1),
    ir_exp(Env1, E2, V2, NewEnv),
    [V = VV].

ir_exp(Env, Exp, V, NewEnv) -->
    ir_cond(Env, Exp, True, False, NewEnv),
    
    [ block(True),
      jump(End) ],
    
    [ block(False),
      jump(End) ],
    
    [ block(End), V = phi(boolean, [(1, True), (0, False)]) ].
    
ir_exp(_, E, _, _) --> { writeln(fail:E), halt }.

ir_cond(Env, not(Exp), LabTrue, LabFalse, NewEnv) -->
    ir_cond(Env, Exp, LabFalse, LabTrue, NewEnv).

ir_cond(Env, E1 && E2, LabTrue, LabFalse, NewEnv) -->
    ir_exp(Env, E1, V1, Env1),
    [ if(V1, Second, LabFalse) ],
    
    [ block(Second) ],
    ir_exp(Env1, E2, V2, NewEnv),
    [ if(V2, LabTrue, LabFalse) ].

ir_cond(Env, E1 '||' E2, LabTrue, LabFalse, NewEnv) -->
    ir_exp(Env, E1, V1, Env1),
    [ if(V1, LabTrue, Second) ],
    
    [ block(Second) ],
    ir_exp(Env1, E2, V2, NewEnv),
    [ if(V2, LabTrue, LabFalse) ].

ir_cond(Env, Exp, LabTrue, LabFalse, NewEnv) -->
    ir_exp(Env, Exp, V, NewEnv),
    [ if(V, LabTrue, LabFalse) ].
%%%%%%%%%%%%%%%%%%
%%% STATEMENTS %%%
%%%%%%%%%%%%%%%%%%

ir_stmts(Env, Stmts, NewEnv) --> dcg_foldl(ir_stmt, Env, Stmts, NewEnv).

% ir_stmt( InEnv, Statement, OutEnv )
% ir_stmt(_, S, _) --> { writeln(S), fail }.

ir_stmt(Env, skip, Env) --> [].

ir_stmt(InEnv, block(Stmts), NewEnv) -->
    { TempEnv = InEnv.reset() },
    ir_stmts(TempEnv, Stmts, OutEnv),
    { NewEnv = InEnv.add_ask(OutEnv.ask).add_mod_set(OutEnv.mod).put(last_block, OutEnv.last_block) }.

ir_stmt(InEnv, Id = Exp, OutEnv) -->
    ir_exp(InEnv, Exp, V, ExpEnv),
    { OutEnv = ExpEnv.add_mod(Id, V) }.

ir_stmt(Env, return, Env) --> [ret].
ir_stmt(Env, return(Exp), NewEnv) -->
    ir_exp(Env, Exp, V, NewEnv),
    [ret(Env.return_type, V)].

ir_stmt(InEnv, incr(Id), OutEnv) --> ir_stmt(InEnv, Id = var(Id) + int(1), OutEnv).
ir_stmt(InEnv, decr(Id), OutEnv) --> ir_stmt(InEnv, Id = var(Id) - int(1), OutEnv).


ir_stmt(Env, decl(_, []), Env) --> [].
ir_stmt(Env, decl(Type, [ noinit(Id) | T ]), NewEnv) -->
    { Type = int -> Def = 0
    ; Type = boolean -> Def = 0
    ; Type = string -> Def = "" },
    ir_stmt(Env.add_var(Id, Type).add_create(Id, Def), decl(Type,T), NewEnv).


ir_stmt(Env, decl(Type, [ init(Id, Exp) | T ]), NewEnv) -->
    ir_exp(Env, Exp, V, ExpEnv),
    ir_stmt(ExpEnv.add_var(Id, Type).add_create(Id, V), decl(Type,T), NewEnv).


ir_stmt(Env, expstmt(Exp), NewEnv) -->
    ir_exp(Env, Exp, _, NewEnv).

ir_stmt(Env, if(If, Then), NewEnv) --> ir_stmt(Env, if(If, Then, skip), NewEnv).
ir_stmt(Env, if(If, Then, Else), NewEnv) -->
    ir_exp(Env, If, V, CondEnv),
    [ if(V, ThenBlock, ElseBlock) ],
    
    { EmptyEnv = CondEnv.reset() },
    
    ir_block(EmptyEnv, ThenBlock, ThenEnv),
    ir_stmt(ThenEnv, Then, PostThenEnv),
    [ jump(EndBlock) ],
    
    ir_block(EmptyEnv, ElseBlock, ElseEnv),
    ir_stmt(ElseEnv, Else, PostElseEnv),
    [ jump(EndBlock) ],
    
    ir_block(Env, EndBlock, PostEnv),
    ir_merge_if(PostEnv.add_ask(CondEnv.ask), PostThenEnv, PostElseEnv, NewEnv).
    

ir_stmt(Env, while(While, Do), NewEnv) -->
    { EmptyEnv = Env.reset() },
    
    [ jump(CondBlock) ],
    
    ir_block(EmptyEnv, DoBlock, DoEnv),
    ir_stmt(DoEnv, Do, PostDoEnv),
    [ jump(CondBlock) ],
    
    ir_block(EmptyEnv, CondBlock, _),
    ir_while_merge(Env, PostDoEnv, MergeEnv),
    ir_exp(MergeEnv, While, V, CondEnv),
    [ if(V, DoBlock, EndBlock) ],
    
    ir_block(CondEnv, EndBlock, NewEnv)
. % while

% ir_stmt(S) --> { writeln(S), fail }.

ir_block(Env, Label, Env.put(last_block, Label)) --> [ block(Label) ].


%%%%%%%%%%%%%%%
%%% PROGRAM %%%
%%%%%%%%%%%%%%%

ir_args([], _{}, []).
ir_args([(Id,Type) | T], SS, [(Reg,Type) | TT]) :-
    ir_args(T, S, TT),
    SS = S.put(Id,Reg).


ir_fun(InEnv, topdef(Ret, Fun, Args, Body)) -->
    { format(user_error, "compiling function: ~w : ~w -> ~w~n", [Fun, Args, Ret]) },
    {
        ir_env(Ret, InEnv.functions, InEnv.strings, Env),
        ir_args(Args, Mod, NArgs),
        FunEnv = Env.add_mod_set(Mod).put(last_block,StartBlock),
        phrase(ir_stmts(FunEnv, Body, _), Code1, [])
    },
    % last block can be empty due to returns in bramches.
    { append( [block(StartBlock) | Code1], [unreachable], Code) },
    [ function(Ret, Fun, NArgs, Code) ].

ir_fun_decls(_{}) --> [].
ir_fun_decls(Decls) -->
    { Fun = Decls.get(Key), del_dict(Key, Decls, Fun, Decls2) },
    ({ Fun.extern = false } ; [ decl(Key, Fun.return, Fun.args) ]),
    ir_fun_decls(Decls2).

ir_str_decl(Str1 - Lab - Len) -->
    { string_concat(Str1, "\\00", Str) },
    [ string(Str, Lab, Len) ].



program(Env, Program) -->
    ir_fun_decls(Env.functions),
    dgc_map(ir_str_decl, Env.strings),
    dgc_map(ir_fun(Env), Program).


ir_program(Env, Program, IR) :-
    phrase(program(Env, Program), IR), !
.




