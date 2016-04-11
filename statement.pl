:- module(statement, [stmt_monad/4, correct//2]).

:- use_module(utils).
:- use_module(environment).
:- use_module(expression).
:- use_module(library(dcg/basics)).

stmt_monad(Fun, Env, RetType, statement{
    function_name: Fun,
    env: Env,
    return_type: RetType,
    returned: false
}).

M.epush() := M.put(env, M.env.push()).
M.epop()  := M.put(env, M.env.pop()).


corrects([], []) --> [].
corrects([H|T], [NH|NT]) --> correct(H, NH), corrects(T, NT).


well_typed(Exp, NExp) --> well_typed(Exp, _, NExp).

well_typed(Exp, Type, NExp) -->
    get_state(S),
    { types(S.env, Exp, Type, NExp) }.

expect_type(Type, Exp, NExp) -->
    get_state(S),
    { expect_type(S.env, Exp, Type, NExp) }.

merge_return(S1,S2), [NS] --> [S],
    { S1.returned = true, S2.returned = true -> Ret = true ; Ret = false },
    { NS = S.put(returned, Ret) }.

%%% SIMPLE STATEMENTS %%%

correct(skip, skip) --> [].
correct(return, return) -->
    get_state(S),
    ({ S.return_type = void } ->
        put_state(S.put(returned, true))
    ; { fail("void return when ~w expected", [S.return_type]) })
    .
    
correct(return(Exp), return(NExp)) -->
    get_state(S),
    well_typed(Exp, Type, NExp),
    ( { S.return_type = Type } ->
        put_state(S.put(returned, true))
    ; { fail("return of type ~w expected but expression ~w of type ~w found", [S.return_type, Exp, Type]) }).

correct(expstmt(Exp), expstmt(NExp)) --> well_typed(Exp, NExp).

correct(incr(Id), incr(Id)) -->
    get_state(S),
    ( {S.env.get_var(Id).type = int} ->
        put_state(S)
    ; { fail("cannot increment non-integer variable ~w", [Id]) }).

correct(decr(Id), decr(Id)) -->
    get_state(S),
    ( {S.env.get_var(Id).type = int} ->
        put_state(S)
    ; { fail("cannot decrement non-integer variable ~w", [Id]) }).
    
correct(Id = Exp, Id = NExp) -->
    get_state(S),
    ( {VarInfo = S.env.get_var(Id)} ->
        expect_type(VarInfo.type, Exp, NExp)
    ; {fail("variable ~w not declared", [Id])} ).


%%% CONTROL STRUCTURES %%%
correct(block(Stmts), block(NStmts)) -->
    do_state(epush()),
    corrects(Stmts, NStmts),
    do_state(epop()).
% M.correct(block(Stmts), block(NStmts)) := M.epush().corrects(Stmts, NStmts).epop().


correct(if(If, Then), NIf) --> correct(if(If, Then, skip), NIf).

correct(if(true, Then, Else), NThen) -->
    do_state(epush()),
    correct(Else, _),
    correct(Then, NThen),
    do_state(epop()).

correct(if(false, Then, Else), NElse) -->
    do_state(epush()),
    correct(Then, _),
    correct(Else, NElse),
    do_state(epop()).

correct(if(If, Then, Else), if(NIf, NThen, NElse)) -->
    expect_type(boolean, If, NIf),
    do_state(epush()),
    get_state(S),
    
    correct(Then, NThen),
    do_state(epop()),
    get_state(S1),
    
    put_state(S),
    correct(Else, NElse),
    do_state(epop()),
    get_state(S2),
    
    merge_return(S1, S2).

correct(while(true, Do), while(true, NDo)) -->
    do_state(epush()),
    correct(Do, NDo),
    do_state(epop()).

correct(while(While, Do), while(NWhile,NDo)) -->
    expect_type(boolean, While, NWhile),
    do_state(epush()),
    correct(Do, NDo),
    get_state(S),
    do_state(epop()),
    do_state(put(returned, S.returned)).


%%% DECLARATIONS %%%
correct(decl(Type, Decls), decl(Type, NDecls)) -->
    decls_correct(Type, Decls, NDecls).

decls_correct(_, [], []) --> [].
decls_correct(Type, [H|T], [NH|NT]) -->
    decl_correct(Type, H, NH),
    decls_correct(Type, T, NT).

decl_correct(Type, init(Id, Exp), init(Id, NExp)) -->
    expect_type(Type, Exp, NExp),
    decl_correct(Type, noinit(Id), _).

decl_correct(Type, noinit(Id), init(Id, V)) -->
    get_state(S),
    { can_shadow(S.env, Id) -> true ; fail("variable ~w already declared", [Id]) },
    { Type = int -> V = int(0)
    ; Type = boolean -> V = false
    ; Type = string -> V = str("") },
    put_state(S.put(env, S.env.add_var(Id, Type))).















