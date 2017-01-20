:- module(environment, [
    emptyenv/1,
    push/2, pop/2,
    add_var/4, get_var/3, add_class//3,
    can_shadow//1,
    op(600, fx, pushed), pushed//1
]).

emptyenv( environment{
    classes: classes{},
    functions : [
        printInt    - fun{ return: void, args: [int], extern: true },
        printString - fun{ return: void, args: [string], extern: true },
        concat      - fun{ return: string, args: [string, string], extern: true },
        readInt     - fun{ return: int, args: [], extern: true },
        readString  - fun{ return: string, args: [], extern: true },
        malloc      - fun{ return: string, args: [int], extern: true }
    ],
    strings : []
}).

% GLOBAL CONTEXT
M.add_fun(Fun, Type, ArgTypes) := M.put(functions, [Fun - FunInfo | M.functions]) :-
    FunInfo = fun{ return: Type, args: ArgTypes, extern: false }.

M.add_string(Str) := M.put(strings, SS) :-
    union([Str - _Label - _Length - _Index], M.strings, SS).

M.add_class(Class, Fields, Methods) := M.put(classes/Class, ClassInfo) :-
    ClassInfo = class{ fields: Fields, methods: Methods }.

M.type_size(class(Class)) := Size :-
    length(M.classes.Class.fields, Len),
    Size is Len * 4.
_M.type_size(int) := 4.
_M.type_size(boolean) := 4.
_M.type_size(string) := 4.
_M.type_size(ref(_)) := 4.
_M.type_size(array(_)) := 8.

% FUNCTION CONTEXT
M.enter_function(Fun, Ret) :=
    M.put(returned, false).put(return_type, Ret).put(function_name, Fun)
     .put(stack,[vars{}]).

M.exit_function() :=
    M.del(returned).del(return_type).del(function_name).

M.push() := M.put(stack, [vars{} | M.stack]).
M.pop() := M.put(stack, Stack) :- M.stack = [_ | Stack].

M.add_var(Id, Type) := M.put(stack, Stack) :- 
    VarInfo = var{ type : Type },
    M.stack = [H | T],
    Stack = [ H.put(Id, VarInfo) | T].

E.get_var(Id) := VarInfo :- member(Block, E.stack), VarInfo = Block.get(Id), !.

can_shadow(Id) --> get_state(S), { S.stack = [H|_] -> \+ H ? get(Id) ; true }.

E.merge(A, B) := E.put(returned, Ret).put(strings, SS) :-
    union(A.strings, B.strings, SS),
    A.returned = true, B.returned = true -> Ret = true ; Ret = false.

:- module_transparent 'pushed'//1.
pushed(I) --> do_state(push()), I, do_state(pop()).

% METHOD CONTEXT
M.enter_method(Class, Meth, Ret) :=
    M.put(returned, false).put(return_type, Ret).put(function_name, Meth)
     .put(stack,[vars{}]).put(caller_class, Class).
M.exit_method() :=
    M.del(returned).del(return_type).del(function_name).del(caller_class).del(stack).del(caller_class).
