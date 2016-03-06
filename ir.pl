:- module(ir, [ir_exp//3, ir_stmt//3, ir_program//1]).

:- use_module(eval_state).

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
    [ V = call(Fun, ArgVs) ].

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



ir_stmts(Env, [], Env) --> [].
ir_stmts(Env, [H|T], NewEnv) -->
    ir_stmt(Env, H, Env2),
    ir_stmts(Env2, T, NewEnv).

ir_stmt(Env, block(Stmts), NewEnv) -->
    [block(_)],
    ir_stmts(Env.push(), Stmts, Env2),
    { NewEnv = Env2.pop() }.

ir_stmt(Env, Id = Exp, NewEnv) -->
    ir_exp(Env, Exp, V),
    { NewEnv = Env.set_var(Id, V) }.

ir_stmt(Env, decl(_, []), Env) --> [].
ir_stmt(Env, decl(_, [ noinit(Id) | T ]), NewEnv) -->
    ir_stmt(Env.add_var(Id,_), decl(_,T), NewEnv).
ir_stmt(Env, decl(_, [ init(Id, Exp) | T ]), NewEnv) -->
    ir_exp(Env, Exp, V),
    ir_stmt(Env.add_var(Id,V), decl(_,T), NewEnv).

ir_stmt(Env, expstmt(Exp), Env) -->
    ir_exp(Env, Exp, _).



ir_fun(topdef(Ret, Fun, Args, Body)) -->
    { new_eval_state(St) },
    [ fun(Ret, Fun, Args) ],
    ir_stmt(St, block(Body), _),
    [ endfun ].


ir_program([]) --> [].
ir_program([H|T]) --> ir_fun(H), ir_program(T).
