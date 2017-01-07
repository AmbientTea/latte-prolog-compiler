:- module(frontend, [check/3]).
:- use_module(utils).
:- use_module(environment).
:- use_module(expression).
:- use_module(statement).
:- use_module(program).

check(Tree, Env, NTree) :-
    correct_program(Tree, NTree, [], [Env]).
