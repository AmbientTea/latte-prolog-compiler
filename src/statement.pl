:- module(statement, [correct//2, can_shadow//1]).

:- use_module(utils).
:- use_module(environment/environment).
:- use_module(expression).
:- use_module(library(dcg/basics)).
:- use_module(leftval).


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
    ; get_state(S), { throw(bad_void_return(S.return_type)) }.

correct(return(_Exp), _NRet) -->
    ask_state(return_type, void),
    { throw(void_exp_return) }.
    
correct(return(Exp), return(Type, NExp)) -->
    ask_state(return_type, Type),
    ( expect_type(Exp, Type, NExp) ->
        do_state(put(returned, true))
    ; get_state(S), { throw(bad_return(Type, S.return_type, Exp)) }).

correct(expstmt(Exp), expstmt(NExp)) --> types(Exp, _, NExp).

correct(incr(Exp), Left = Left + int(1)) -->
    leftval(Exp, Type, Left),
    { Type == int or_else throw(bad_increment(Exp, Type)) }.

correct(decr(Exp), Left = Left - int(1)) -->
    leftval(Exp, Type, Left),
    { Type == int or_else throw(bad_decrement(Exp, Type)) }.
    
correct(LeftVal = Exp, NLeftVal = NExp) -->
    leftval(LeftVal, LType, NLeftVal),
    expect_type(Exp, LType, NExp).

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
    !, pushed correct(Do, NDo),
    % no 'break' instruction so return not needed
    do_state put(returned, true).

correct(while(While, Do), while(NWhile,NDo)) -->
    expect_type(While, boolean, NWhile),
    pushed correct(Do, NDo).

correct(for(Type, Var, Arr, Do), for(Type, Var, NArr, NDo)) -->
    types(Arr, ArrType, NArr),
    { ArrType = ref(array(AType)) or_else throw(bad_iteratee(ArrType, Arr)) },
    { AType = Type or_else throw(bad_iterator(Type, AType)) },
    pushed ( do_state add_var(Var, Type),
             correct(Do, NDo) ).

%%% DECLARATIONS %%%

correct(decl(void, Decls), _NDecls) -->
    { throw(void_decl(Decls)) }.

correct(decl(Type, Decls), decl(Type, NDecls)) -->
    dcg_map( decl_correct(Type), Decls, NDecls).

decl_correct(Type, init(Id, Exp), init(Id, NExp)) -->
    expect_type(Exp, Type, NExp),
    decl_correct(Type, noinit(Id), _).

decl_correct(Type, noinit(Id), init(Id, V)) -->
    can_shadow(Id) ->
        { Type = int -> V = int(0)
        ; Type = boolean -> V = false
        ; Type = string -> V = str("")
        ; Type = ref(_) -> V = null
        ; Type = array(_) -> V = null },
        do_state(add_var(Id, Type))
    ; { throw(dupl_decl(Id)) }.















