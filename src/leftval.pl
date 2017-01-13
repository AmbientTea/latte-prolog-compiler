:- module(leftval, [leftval//3]).
:- use_module(utils).
:- use_module(environment).


leftval(var(Id), Type, var(Type, Id)) -->
    ask_state(get_var(Id), VarInfo) ->
        { Type = VarInfo.type }
    ; { throw(not_declared(Id)) }.






