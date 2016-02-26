:- module(frontend, [check/1]).
:- use_module(utils).
:- use_module(environment).
:- use_module(expression).
:- use_module(statement).
:- use_module(program).

stripf(F, A) :- F =.. [_|Arg], A =.. [','|Arg].

fundef( topdef(RetT, Id, Args, _), (RetT, Id, ArgTs) ) :- maplist(snd, Args, ArgTs).
envFunInfo(_) :- false.

check(program(Tree)) :- correct_program(Tree), writeln(ok).
check(stmt(Stmt)) :- writeln(stmt), emptyenv(Env), correct(Env, Stmt, _), writeln(ok).
check(exp(Exp)) :- emptyenv(Env), types(Env, Exp, Type), writeln(type: Type).
