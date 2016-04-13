:- module(statement, [correct//2]).

:- use_module(utils).
:- use_module(environment).
:- use_module(expression).
:- use_module(library(dcg/basics)).


corrects([], []) --> [].
corrects([H|T], [NH|NT]) --> correct(H, NH), corrects(T, NT).

well_typed(Exp, NExp) --> types(Exp, _, NExp).

merge_return(S1,S2), [NS] --> [S],
    { (S1.returned = true, S2.returned = true) -> Ret = true ; Ret = false },
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
    types(Exp, Type, NExp),
    ( { S.return_type = Type } ->
        put_state(S.put(returned, true))
    ; { fail("return of type ~w expected but expression ~w of type ~w found", [S.return_type, Exp, Type]) }).

correct(expstmt(Exp), expstmt(NExp)) --> well_typed(Exp, NExp).

correct(incr(Id), incr(Id)) -->
    get_state(S),
    ( {S.get_var(Id).type = int} ->
        put_state(S)
    ; { fail("cannot increment non-integer variable ~w", [Id]) }).

correct(decr(Id), decr(Id)) -->
    get_state(S),
    ( {S.get_var(Id).type = int} ->
        put_state(S)
    ; { fail("cannot decrement non-integer variable ~w", [Id]) }).
    
correct(Id = Exp, Id = NExp) -->
    get_state(S),
    ( {VarInfo = S.get_var(Id)} ->
        expect_type(Exp, VarInfo.type, NExp)
    ; {fail("variable ~w not declared", [Id])} ).


%%% CONTROL STRUCTURES %%%
correct(block(Stmts), block(NStmts)) -->
    do_state(push()),
    corrects(Stmts, NStmts),
    do_state(pop()).

correct(if(If, Then), NIf) --> correct(if(If, Then, skip), NIf).

correct(if(true, Then, Else), NThen) -->
    do_state(push()),
    correct(Else, _),
    correct(Then, NThen),
    do_state(pop()).

correct(if(false, Then, Else), NElse) -->
    do_state(push()),
    get_state(S),
    correct(Then, _),
    put_state(S),
    correct(Else, NElse),
    do_state(pop()).

correct(if(If, Then, Else), if(NIf, NThen, NElse)) -->
    expect_type(If, boolean, NIf),
    
    do_state(push()),
    local(correct(Then, NThen), S1),
    local(correct(Else, NElse), S2),
    do_state(pop()),
    
    merge_return(S1, S2).

correct(while(true, Do), while(true, NDo)) -->
    do_state(push()),
    correct(Do, NDo),
    do_state(pop()).

correct(while(While, Do), while(NWhile,NDo)) -->
    expect_type(While, boolean, NWhile),
    do_state(push()),
    correct(Do, NDo),
    get_state(S),
    do_state(pop()),
    do_state(put(returned, S.returned)).


%%% DECLARATIONS %%%
correct(decl(Type, Decls), decl(Type, NDecls)) -->
    decls_correct(Type, Decls, NDecls).

decls_correct(_, [], []) --> [].
decls_correct(Type, [H|T], [NH|NT]) -->
    decl_correct(Type, H, NH),
    decls_correct(Type, T, NT).

decl_correct(Type, init(Id, Exp), init(Id, NExp)) -->
    expect_type(Exp, Type, NExp),
    decl_correct(Type, noinit(Id), _).

decl_correct(Type, noinit(Id), init(Id, V)) -->
    get_state(S),
    { can_shadow(S, Id) -> true ; fail("variable ~w already declared", [Id]) },
    { Type = int -> V = int(0)
    ; Type = boolean -> V = false
    ; Type = string -> V = str("") },
    do_state(add_var(Id, Type)).















