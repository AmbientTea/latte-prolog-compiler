:- module(statement, [correct//2, can_shadow//1]).

:- use_module(utils).
:- use_module(environment).
:- use_module(expression).
:- use_module(library(dcg/basics)).


corrects([], []) --> !.
corrects([H|T], [NH|NT]) --> correct(H, NH), !, corrects(T, NT).

merge_return(S1,S2), [NS] --> [S],
    { (S1.returned = true, S2.returned = true) -> Ret = true ; Ret = false },
    { union(S1.strings, S2.strings, Strings) },
    { NS = S.put(returned, Ret).put(strings, Strings) }.

%%% SIMPLE STATEMENTS %%%

correct(skip, skip) --> !.
correct(return, return) -->
    ask_state(return_type, void) ->
        do_state(put(returned, true))
    ; get_state(S), { fail("void return when ~w expected", [S.return_type]) }.
    
correct(return(Exp), return(Type, NExp)) -->
    types(Exp, Type, NExp),
    ( ask_state(return_type, Type) ->
        do_state(put(returned, true))
    ; get_state(S), { fail("return of type ~w expected but expression ~w of type ~w found", [S.return_type, Exp, Type]) }).

correct(expstmt(Exp), expstmt(NExp)) --> types(Exp, _, NExp).

correct(incr(Id), Id = var(Id) + int(1)) -->
    ask_state(get_var(Id), VarInfo), { VarInfo.type = int }, !
    ; { fail("cannot increment non-integer variable ~w", [Id]) }.

correct(decr(Id), Id = var(Id) - int(1)) -->
    ask_state(get_var(Id), VarInfo), { VarInfo.type = int }, !
    ; { fail("cannot decrement non-integer variable ~w", [Id]) }.
    
correct(Id = Exp, Id = NExp) -->
    ask_state(get_var(Id), VarInfo) ->
        expect_type(Exp, VarInfo.type, NExp)
    ; { fail("variable ~w not declared", [Id]) }.


%%% CONTROL STRUCTURES %%%
correct(block(Stmts), block(NStmts)) --> pushed corrects(Stmts, NStmts).

correct(if(If, Then), NIf) --> correct(if(If, Then, skip), NIf).

correct(if(true, Then, Else), NThen) -->
    !, local (pushed correct(Else, _)),
    !, pushed correct(Then, NThen).

correct(if(false, Then, Else), NElse) -->
    !, local (pushed correct(Then, _)),
    !, pushed correct(Else, NElse).

correct(if(If, Then, Else), if(NIf, NThen, NElse)) -->
    expect_type(If, boolean, NIf),

    pushed local(correct(Then, NThen), S1),
    pushed local(correct(Else, NElse), S2),
    
    merge_return(S1, S2).

correct(while(true, Do), while(true, NDo)) -->
    !, pushed correct(Do, NDo).

correct(while(While, Do), while(NWhile,NDo)) -->
    expect_type(While, boolean, NWhile),
    pushed correct(Do, NDo).


%%% DECLARATIONS %%%
correct(decl(Type, Decls), decl(Type, NDecls)) -->
    dcg_map( decl_correct(Type), Decls, NDecls).

decl_correct(Type, init(Id, Exp), init(Id, NExp)) -->
    expect_type(Exp, Type, NExp),
    decl_correct(Type, noinit(Id), _).

decl_correct(Type, noinit(Id), init(Id, V)) -->
    can_shadow(Id) ->
        { Type = int -> V = int(0)
        ; Type = boolean -> V = false
        ; Type = string -> V = str("") },
        do_state(add_var(Id, Type))
    ; { fail("variable ~w already declared", [Id]) }.















