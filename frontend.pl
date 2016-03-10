:- module(frontend, [check/2]).
:- use_module(utils).
:- use_module(environment).
:- use_module(expression).
:- use_module(statement).
:- use_module(program).

stripf(F, A) :- F =.. [_|Arg], A =.. [','|Arg].

fundef( topdef(RetT, Id, Args, _), (RetT, Id, ArgTs) ) :- maplist(snd, Args, ArgTs).
envFunInfo(_) :- false.

check(program(Tree), Env) :- correct_program(Tree, Env).
check(stmt(Stmt), Env) :-
    emptyenv(Env),
    stmt_monad(..., Env, void, M),
    _ = M.epush().correct(Stmt).
check(exp(Exp), Env) :- emptyenv(Env), types(Env, Exp, Type, _), writeln(type: Type).
