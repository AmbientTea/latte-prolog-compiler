:- module(statement, [stmt_monad/4]).

:- use_module(utils).
:- use_module(environment).
:- use_module(expression).

stmt_monad(Fun, Env, RetType, statement{
    function_name: Fun,
    env: Env,
    return_type: RetType,
    returned: false
}).

M.corrects([], []) := M.
M.corrects([H|T], [NH|NT]) := M.correct(H, NH).corrects(T, NT).

M.epush() := M.put(env, M.env.push()).
M.epop()  := M.put(env, M.env.pop()).

M.well_typed(Exp, NExp) := M :- types(M.env, Exp, _, NExp).
M.well_typed(Exp, Type, NExp) := M :- types(M.env, Exp, Type, NExp).
M.expect_type(Type, Exp, NExp) := M :- expect_type(M.env, Exp, Type, NExp). 

M.merge_return(M1,M2) := M.put(returned, Ret) :-
    M1.returned = true, M2.returned = true -> Ret = true ; Ret = false.

%%% SIMPLE STATEMENTS %%%

M.correct(skip, skip) := M.
M.correct(return, return) := M.put(returned, true) :-
    M.return_type = void -> true
    ; fail("void return when ~w expected", [M.return_type]).

M.correct(return(Exp), return(NExp)) := M.put(returned, true) :-
    types(M.env, Exp, Type, NExp),
    ( M.return_type = Type -> true
    ; fail("return of type ~w expected but expression ~w of type ~w found", [M.return_type, Exp, Type]) ).
    
M.correct(expstmt(Exp), expstmt(NExp)) := M.well_typed(Exp, _, NExp).

M.correct(incr(Id), incr(Id)) := M :-
    M.env.get_var(Id).type = int -> true
    ; fail("cannot increment non-integer variable ~w", [Id]).
M.correct(decr(Id), decr(Id)) := M :-
    M.env.get_var(Id).type = int -> true
    ; fail("cannot decrement non-integer variable ~w", [Id]).

M.correct(Id = Exp, Id = NExp) := M.expect_type(VarInfo.type, Exp, NExp) :-
    ( VarInfo = M.env.get_var(Id) -> true ; fail("variable ~w not declared", [Id]) ). 

%%% CONTROL STRUCTURES %%%

M.correct(block(Stmts), block(NStmts)) := M.epush().corrects(Stmts, NStmts).epop().

M.correct(while(true, Do), while(true, NDo)) := M.epush().correct(Do, NDo).epop() :- !.
M.correct(while(While, Do), while(NWhile,NDo)) :=
    M.expect_type(boolean, While, NWhile).epush().correct(Do, NDo).epop().put(returned, M.returned).

M.correct(if(true, Then, Else), NThen) := M.epush().correct(Then, NThen).epop() :- !, _ = M.epush().correct(Else, _).
M.correct(if(false, Then, Else), NElse) := M.epush().correct(Else, NElse).epop() :- !, _ = M.epush().correct(Then, _).
M.correct(if(If, Then, Else), if(NIf, NThen, NElse)) := M.expect_type(boolean, If, NIf).merge_return(
    M.epush().correct(Then, NThen).epop(),
    M.epush().correct(Else, NElse).epop()).

M.correct(if(If, Then), NIf) := M.correct(if(If, Then, skip), NIf).

%%% DECLARATIONS %%%

M.correct(decl(Type, Decls), decl(Type, NDecls)) := M.decls_correct(Type, Decls, NDecls).

M.decls_correct(_, [], []) := M.
M.decls_correct(Type, [H|T], [NH|NT]) := M.decl_correct(Type, H, NH).decls_correct(Type, T, NT).

M.decl_correct(Type, init(Id, Exp), init(Id, NExp)) := M.expect_type(Type, Exp, NExp).decl_correct(Type, noinit(Id), _).
M.decl_correct(Type, noinit(Id), init(Id, V)) := M.put(env, M.env.add_var(Id, Type)) :-
    (can_shadow(M.env, Id) -> true ; fail("variable ~w already declared", [Id])),
    ( Type = int -> V = int(0)
    ; Type = boolean -> V = false
    ; Type = string -> V = str("")).





