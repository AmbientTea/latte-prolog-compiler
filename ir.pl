:- module(ir, [ir_exp//3, ir_stmt//3, ir_program/3]).

:- use_module(eval_state).
:- use_module(utils).


merge_env(Env1, Env2, Br1, Br2, NewEnv) -->
    { Env1.vars = [Top1|_], Env2.vars = [Top2|_] } -> (
        merge_env_level(Top1, Top2, Br1, Br2, NewTop),
        merge_env(Env1.pop(), Env2.pop(), Br1, Br2, NewEnv1),
        { NewEnv = NewEnv1.put(vars, [NewTop | NewEnv1.vars]) }
    ) ; [], { Env1 = NewEnv }.


merge_env_level(Env1, Env2, Br1, Br2, NewEnv) -->
    { V1 = Env1.get(K), V2 = Env2.get(K) } -> (
        { del_dict(K, Env1, _, NEnv1), del_dict(K, Env2, _, NEnv2) },
        merge_env_level(NEnv1, NEnv2, Br1, Br2, NewEnv1),
        ( { V1 == V2 } ->
            [], { NewEnv = NewEnv1.put(K, V1) }
            ;
            [ V3 = phi([V1, Br1], [V2, Br2]) ],
            { NewEnv = NewEnv1.put(K, V3) }
        )
    ) ; [], { NewEnv = _{} }.
    
    
    


%%% EXPRESSIONS %%%

ir_exps(_, [], []) --> [].
ir_exps(Env, [H|T], [HV|TV]) -->
    ir_exp(Env, H, HV),
    ir_exps(Env, T, TV).

ir_exp(_, int(I), I) --> [].
ir_exp(_, str(S), S) --> [].
ir_exp(_, true, true) --> [].
ir_exp(_, false, false) --> [].
ir_exp(Env, var(Id), Env.get_var(Id)) --> [].

ir_exp(Env, app(Fun, Args), V) -->
    ir_exps(Env, Args, ArgVs),
    { Info = Env.env.functions.Fun },
    { zip(ArgVs, (Info.args), ArgsF) },
    ({ Info.return = void } ->
        [ call(Fun, ArgsF) ]
      ; [ V = call(Info.return, Fun, ArgsF) ]).

ir_exp(Env, E, V) -->
    { E =.. [Op, E1, E2], member(Op, [+,-,*,/,'%',<,>,'<=','>=','!=','==']), VV =.. [Op, V1, V2] },
    ir_exp(Env, E1, V1),
    ir_exp(Env, E2, V2),
    [V = VV].

ir_exp(Env, '||'(E1,E2), V) -->
    ir_exp(Env, E1, V1),
    [ if(V1, True, Second) ],
    
    [ block(Second) ],
    ir_exp(Env, E2, V2),
    [ if(V2, True, False) ],
    
    [ block(True),
      jump(End) ],
    
    [ block(False),
      jump(End) ],
    
    [ block(End),
      V = phi([(1, True), (0, False)]) ].

%%% STATEMENTS %%%

ir_stmts(Env, [], Env) --> [].
ir_stmts(Env, [H|T], NewEnv) -->
    ir_stmt(Env, H, Env2),
    ir_stmts(Env2, T, NewEnv).

ir_stmt(Env, block(Stmts), NewEnv) -->
    ir_stmts(Env.push(), Stmts, Env2),
    { NewEnv = Env2.pop() }.

ir_stmt(Env, Id = Exp, NewEnv) -->
    ir_exp(Env, Exp, V),
    { NewEnv = Env.set_var(Id, V) }.

ir_stmt(Env, incr(Id), NewEnv ) --> ir_stmt(Env, Id = Id + 1, NewEnv).
ir_stmt(Env, decr(Id), NewEnv ) --> ir_stmt(Env, Id = Id - 1, NewEnv).

ir_stmt(Env, decl(_, []), Env) --> [].
ir_stmt(Env, decl(_, [ noinit(Id) | T ]), NewEnv) -->
    ir_stmt(Env.add_var(Id,_), decl(_,T), NewEnv).
ir_stmt(Env, decl(_, [ init(Id, Exp) | T ]), NewEnv) -->
    ir_exp(Env, Exp, V),
    ir_stmt(Env.add_var(Id,V), decl(_,T), NewEnv).

ir_stmt(Env, expstmt(Exp), Env) -->
    ir_exp(Env, Exp, _).

ir_stmt(Env, if(If, Then, Else), NewEnv ) -->
    ir_exp(Env, If, IfV),
    [ if(IfV, ThenBlock, ElseBlock) ],
    
    [ block(ThenBlock) ],
    ir_stmt(Env.push(), Then, ThenEnv),
    [ jmp(EndBlock) ],
    
    [ block(ElseBlock) ],
    ir_stmt(Env.push(), Else, ElseEnv),
    [ jmp(EndBlock) ],
    
    [ block(EndBlock) ],
    merge_env(ThenEnv.pop(), ElseEnv.pop(), ThenBlock, ElseBlock, NewEnv).
    
    


%%% PROGRAM %%%

ir_args(St, [], [], St).
ir_args(St, [(Id, Type) | T], [(Var, Type) | TT], NSt) :-
    ir_args(St.add_var(Id, Var), T, TT, NSt).
    

ir_fun(Env, topdef(Ret, Fun, Args, Body), fun(Ret, Fun, FArgs, Code)) :-
    new_eval_state(Env, St),
    ir_args(St.push(), Args, FArgs, FSt),
    phrase(ir_stmt(FSt, block(Body), _), Code)
.

ir_program(Env, Prog, Code) :- maplist(ir_fun(Env), Prog, Code).
