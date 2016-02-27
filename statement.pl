:- module(statement, [correct/3]).

:- use_module(utils).
:- use_module(environment).
:- use_module(expression).

%%% HELPERS %%%%
bool_predict(true, true).
bool_predict(false, false).

corrects(Env, Stmts, NEnv) :- foldr(correct, Env, Stmts, NEnv).

%%% DECLARATIONS %%%

decl_correct(Env, Type, init(Id, Exp), MEnv) :-
    expect_type(Env, Exp, Type),
    decl_correct(Env, Type, noinit(Id), MEnv).

decl_correct(Env, Type, noinit(Id), MEnv) :-
    ( can_shadow(Env, Id), ! ; fail("variable ~w already declared", [Id])),
    MEnv = Env.add_var(Id, Type).

%%% STATEMENTS %%%

correct(Env, skip, Env).

correct(Env, return, NEnv) :-
    Env.return = void ->
        NEnv = Env.put(returned, true)
    ; fail("void return when ~w expected", [Env.return]).
correct(Env, return(Exp), NEnv) :-
    types(Env, Exp, Type),
    ( Env.return = Type ->
        NEnv = Env.put(returned, true)
    ; fail("return of type ~w expected but expression ~w of type ~w found", [Env.return, Exp, Type]) ).

correct(Env, expstmt(Exp), Env) :- types(Env, Exp, _).

correct(Env, decl(_, []), Env).
correct(Env, decl(Type, [H | T]), NEnv) :-
    decl_correct(Env, Type, H, MEnv),
    correct(MEnv, decl(Type, T), NEnv).

correct(Env, incr(Id), Env) :- expect_type(Env, var(Id), int).
correct(Env, decr(Id), Env) :- expect_type(Env, var(Id), int).

correct(Env, =(Id, Exp), Env) :-
    ( VarInfo = Env.get_var(Id), ! ; fail("variable ~w not declared", [Id]) ),
    expect_type(Env, Exp, VarInfo.type).

correct(Env, block(Stmts), NEnv) :-
    corrects(Env.push(), Stmts, BEnv),
    NEnv = Env.put(returned, BEnv.returned).

correct(Env, while(While, Do), NEnv) :-
    expect_type(Env, While, boolean),
    correct(Env.push(), Do, IEnv),
    ( bool_predict(While, true) ->
      NEnv = Env.put(returned, IEnv.returned)
    ; NEnv = Env
    ).

correct(Env, if(If, Then, Else), NEnv) :-
    expect_type(Env, If, boolean),
    correct(Env.push(), Then, IEnv1),
    correct(Env.push(), Else, IEnv2),
    ( bool_predict(If, true) -> NEnv = Env.put(returned, IEnv1.returned)
    ; bool_predict(If, false) -> NEnv = Env.put(returned, IEnv2.returned)
    ; NEnv = Env.merge(IEnv1, IEnv2)
    ).

correct(Env, if(If, Then), NEnv) :- correct(Env, if(If, Then, skip), NEnv).















