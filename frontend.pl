:- module(frontend, [check/3]).
:- use_module(utils).
:- use_module(environment).
:- use_module(expression).
:- use_module(statement).
:- use_module(program).

stripf(F, A) :- F =.. [_|Arg], A =.. [','|Arg].

fundef( topdef(RetT, Id, Args, _), (RetT, Id, ArgTs) ) :- maplist(snd, Args, ArgTs).
envFunInfo(_) :- false.

check(program(Tree), Env, program(NTree)) :- correct_program(Tree, Env, NTree).
check(stmt(Stmt), Env, stmt(NStmt)) :-
    emptyenv(Env),
    stmt_monad(..., Env, void, M),
    _ = M.epush().correct(Stmt, NStmt).
check(exp(Exp), Env, exp(NExp)) :- emptyenv(Env), types(Env, Exp, Type, NExp), writeln(type: Type).
