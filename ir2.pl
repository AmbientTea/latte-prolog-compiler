:- module(ir2, [ir_program/3]).


ir_env(Type, Funs, ir2{
    ask: _{},
    create: _{},
    mod: _{},
    last_block: 0,
    return_type: Type,
    var_types: _{},
    funs: Funs
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
    _ = E.create.get(Key) ->
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

ir_exps(Env, [], [], Env) --> [].
ir_exps(Env, [H|T], [HV|TV], OutEnv) -->
    ir_exp(Env, H, HV, EOutEnv),
    ir_exps(EOutEnv, T, TV, OutEnv).

% ir_exp( Expression, Value, AskSet )
ir_exp(Env, int(I), I, Env) --> !, [].
ir_exp(Env, var(Id), Reg, Env.add_ask(Id, Reg)) --> !, [].
ir_exp(Env, false, 0, Env) --> !, [].
ir_exp(Env, true, 1, Env) --> !, [].

ir_exp(Env, app(Fun, ArgExps), V, NewEnv) -->
    ir_exps(Env, ArgExps, ArgVals, NewEnv),
    [ V = app(Env.funs.Fun.ret, Fun, ArgVals) ].

ir_exp(Env, E, V, NewEnv) -->
    { E =.. [Op, E1, E2], member(Op, [+,-,*,/,'%',<,>,'<=','>=','!=','==']), VV =.. [Op, V1, V2] }, !,
    ir_exp(Env, E1, V1, Env1),
    ir_exp(Env1, E2, V2, NewEnv),
    [V = VV].

ir_exp(Env, '||'(E1,E2), V, NewEnv) -->
    ir_exp(Env, E1, V1, Env1),
    [ if(V1, True, Second) ],
    
    [ block(Second) ],
    ir_exp(Env1, E2, V2, NewEnv),
    [ if(V2, True, False) ],
    
    [ block(True),
      jump(End) ],
    
    [ block(False),
      jump(End) ],
    
    [ block(End), V = phi(boolean, [(1, True), (0, False)]) ].

ir_exp(Env, '&&'(E1,E2), V, NewEnv) -->
    ir_exp(Env, E1, V1, Env1),
    [ if(V1, Second, False) ],
    
    [ block(Second) ],
    ir_exp(Env1, E2, V2, NewEnv),
    [ if(V2, True, False) ],
    
    [ block(True),
      jump(End) ],
    
    [ block(False),
      jump(End) ],
    
    [ block(End), V = phi(boolean, [(1, True), (0, False)]) ].
    
%%%%%%%%%%%%%%%%%%
%%% STATEMENTS %%%
%%%%%%%%%%%%%%%%%%

ir_stmts(Env, [], Env) --> [].
ir_stmts(InEnv, [H|T], OutEnv) -->
    ir_stmt(InEnv, H, InEnv2), !,
    ir_stmts(InEnv2, T, OutEnv).

% ir_stmt( InEnv, Statement, OutEnv )
% ir_stmt(_, S, _) --> { writeln(S), fail }.

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
    ( Type = int -> Def = 0
    ; Type = boolean -> Def = 0
    ; Type = string -> Def = "" ),
    ir_stmt(Env.add_var(Id, Type).add_create(Id, Def), decl(Type,T), NewEnv).


ir_stmt(Env, decl(Type, [ init(Id, Exp) | T ]), NewEnv) -->
    ir_exp(Env, Exp, V, ExpEnv),
    ir_stmt(ExpEnv.add_var(Id, Type).add_create(Id, V), decl(Type,T), NewEnv).


ir_stmt(Env, expstmt(Exp), NewEnv) -->
    ir_exp(Env, Exp, _, NewEnv).

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
    ir_merge_if(PostEnv, PostThenEnv, PostElseEnv, NewEnv).
    

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


ir_stmt(S) --> { writeln(S), fail }.


ir_block(Env, Label, NewEnv) -->
    [ block(Label) ],
    { NewEnv = Env.put(last_block, Label) }.

%%%%%%%%%%%%%%%
%%% PROGRAM %%%
%%%%%%%%%%%%%%%

ir_args([], _{}, []).
ir_args([(Id,Type) | T], SS, [(Reg,Type) | TT]) :-
    ir_args(T, S, TT),
    SS = S.put(Id,Reg).


ir_fun(InEnv, topdef(Ret, Fun, Args, Body)) -->
    {
        ir_env(Ret, InEnv.functions, Env),
        ir_args(Args, Mod, NArgs),
        phrase(ir_stmts(Env.add_mod_set(Mod), Body, _), Code)
    },
    [ function(Ret, Fun, NArgs, Code) ].




program(_, []) --> [].
program(Env, [H|T]) --> ir_fun(Env, H), !, program(Env, T).





ir_program(Env, Program, IR) :-
    phrase(program(Env, Program), IR)
.



