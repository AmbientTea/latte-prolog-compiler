:- module(environment, [emptyenv/1, push/2, pop/2, add_var/4, get_var/3, can_shadow/2]).

emptyenv( environment{
    return : void,
    functions : functions{
        printInt : fun{ return: void, args: [int], extern: true },
        printString : fun{ return: void, args: [string], extern: true }
    },
    stack : [],
    returned : false
}).

M.add_fun(Fun, Type, ArgTypes) := M.put(functions, M.functions.put(Fun,FunInfo)) :-
    FunInfo = fun{ return: Type, args: ArgTypes, extern: false }.

M.push() := M.put(stack, [vars{} | M.stack]).
M.pop() := M.put(stack, Stack) :- M.stack = [_ | Stack].

M.add_var(Id, Type) := M.put(stack, Stack) :- 
    VarInfo = var{ type : Type },
    M.stack = [H | T],
    Stack = [ H.put(Id, VarInfo) | T].

E.get_var(Id) := VarInfo :- member(Block, E.stack), VarInfo = Block.get(Id), !.

can_shadow(Env, Id) :- Env.stack = [H|_] -> \+ _ = H.get(Id) ; true.

E.merge(A, B) := E.put(returned, Ret) :-
    A.returned = true, B.returned = true -> Ret = true ; Ret = false.
