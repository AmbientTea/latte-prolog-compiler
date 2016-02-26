:- module(statement, [correct/3]).

:- use_module(utils).
:- use_module(environment).
:- use_module(expression).

%%% HELPERS %%%%

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

correct(Env, return, Env) :- Env.return = void, ! ; fail("void return when ~w expected", [Env.return]).
correct(Env, return(Exp), Env) :-
    types(Env, Exp, Type),
    ( Env.return = Type, !
    ; fail("return of type ~w expected but expression ~w of type ~w found", [Env.return, Exp, Type]) ).

correct(Env, expstmt(Exp), Env) :- types(Env, Exp, _).

correct(Env, decl(_, []), Env).
correct(Env, decl(Type, [H | T]), NEnv) :-
    decl_correct(Env, Type, H, MEnv),
    correct(MEnv, decl(Type, T), NEnv).

correct(Env, incr(Id), Env) :- expect_type(Env, id(Id), int).
correct(Env, decr(Id), Env) :- expect_type(Env, id(Id), int).

correct(Env, =(Id, Exp), Env) :-
    ( VarInfo = Env.get_var(Id), ! ; fail("variable ~w not declared", [Id]) ),
    expect_type(Env, Exp, VarInfo.type).

correct(Env, block(Stmts), NEnv) :- corrects(Env.push(), Stmts, NEnv).

correct(Env, while(While, Do), Env) :-
    expect_type(Env, While, boolean),
    correct(Env.push(), Do, _).

correct(Env, if(If, Then, Else), Env) :-
    expect_type(Env, If, boolean),
    correct(Env.push(), Then, _),
    correct(Env.push(), Else, _).

correct(Env, if(If, Then), Env) :- correct(Env, if(If, Then, skip), Env).
