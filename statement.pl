:- module(statement, [stmt_monad/4, correct/3]).

:- use_module(utils).
:- use_module(environment).
:- use_module(expression).

stmt_monad(Fun, Env, RetType, statement{
    function_name: Fun,
    env: Env,
    return_type: RetType,
    returned: false
}).

M.corrects([]) := M.
M.corrects([H|T]) := M.correct(H).corrects(T).

M.epush() := M.put(env, M.env.push()).
M.epop()  := M.put(env, M.env.pop()).

M.well_typed(Exp) := M :- types(M.env, Exp, _).
M.expect_type(Type, Exp) := M :- expect_type(M.env, Exp, Type). 

M.merge_return(M1,M2) := M.put(returned, Ret) :-
    M1.returned = true, M2.returned = true -> Ret = true ; Ret = false.

%%%%%

M.correct(skip) := M.
M.correct(return) := M.put(returned, true) :-
    M.return_type = void -> true
    ; fail("void return when ~w expected", [M.return_type]).

M.correct(return(Exp)) := M.put(returned, true) :-
    types(M.env, Exp, Type),
    ( M.return_type = Type -> true
    ; fail("return of type ~w expected but expression ~w of type ~w found", [M.return_type, Exp, Type]) ).
    
M.correct(expstmt(Exp)) := M.well_typed(Exp).

M.correct(incr(Id)) := M :-
    M.env.get_var(Id).type = int -> true
    ; fail("cannot increment non-integer variable ~w", [Id]).
M.correct(decr(Id)) := M :-
    M.env.get_var(Id).type = int -> true
    ; fail("cannot decrement non-integer variable ~w", [Id]).

M.correct(=(Id, Exp)) := M.expect_type(VarInfo.type, Exp) :-
    ( VarInfo = M.env.get_var(Id) -> true ; fail("variable ~w not declared", [Id]) ). 

M.correct(block(Stmts)) := M.epush().corrects(Stmts).epop().

M.correct(while(true, Do)) := M.epush().correct(Do).epop() :- !.
M.correct(while(While, Do)) := M.expect_type(boolean, While).epush().correct(Do).epop().put(returned, M.returned).

M.correct(if(true, Then, Else)) := M.epush().correct(Then).epop() :- _ = M.epush().correct(Else), !.
M.correct(if(false, Then, Else)) := M.epush().correct(Else).epop() :- _ = M.epush().correct(Then), !.
M.correct(if(If, Then, Else)) := M.expect_type(boolean, If).merge_return(
    M.epush().correct(Then).epop(),
    M.epush().correct(Else).epop()).

M.correct(if(If, Then)) := M.correct(if(If, Then, skip)).

%%% STATEMENTS %%%

M.correct(decl(_, [])) := M.
M.correct(decl(Type, [H|T])) := M.decl_correct(Type, H).correct(decl(Type, T)).


M.decl_correct(Type, init(Id, Exp)) := M.expect_type(Type, Exp).decl_correct(Type, noinit(Id)).

M.decl_correct(Type, noinit(Id)) := M.put(env, M.env.add_var(Id, Type)) :-
    can_shadow(M.env, Id) -> true ; fail("variable ~w already declared", [Id]).





