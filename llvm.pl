:- module(llvm, [llvm_compile/2]).

:- use_module(library(dcg/basics)).

llvm_type(int) --> "i32".
llvm_type(boolean) --> "i1".
llvm_type(void) --> "void".
llvm_type(string) --> "i8*".

llvm_args([]) --> [].
llvm_args([(Var,_Id,Type) | T]) -->
    llvm_type(Type), " %", atom(Var),
    ( { T = [] } -> [] ; ", ", llvm_argS(T)).

llvm_op(+, "add", "i32", "i32").
llvm_op(-, "sub", "i32", "i32").
llvm_op(*, "mul", "i32", "i32").
llvm_op(/, "div", "i32", "i32").

llvm_op(>, "icmp ge", "i32", "i1").

llvm_compile(In, Out) :- phrase(llvm_compile(In), Out).

llvm_compile([]) --> [].
llvm_compile([H|T]) --> llvm_fun(H), llvm_compile(T).

llvm_fun(fun(Type, Fun, Args, Body)) -->
    "define ", llvm_type(Type), " @", atom(Fun), "(", llvm_args(Args), "){",
    llvm_stmts(Body),
    "\n}". 


indent(block(_)) --> "".
indent(_) --> "    ".
llvm_stmts([]) --> [].
llvm_stmts([H|T]) --> "\n", indent(H), llvm_stmt(H), !, llvm_stmts(T).


llvm_stmt(S) --> { writeln(left:S), fail }.

llvm_stmt(block(B)) --> atom(B), ":".

llvm_stmt(V = call(Fun, Args)) -->
    atom(V), " = call ", /* type */ "@", atom(Fun), "(", llvm_args(Args), ")".

llvm_stmt(V = OpE) -->
    { OpE =.. [Op, V1, V2], llvm_op(Op, LLOp, InT, _) },
    atom(V), " = ", LLOp, " ", InT, " ", atom(V1), ", ", atom(V2).

llvm_stmt(if(Cond, Lab1, Lab2)) -->
    "br i1 ", atom(Cond), ", label %", atom(Lab1), ", label %", atom(Lab2).

llvm_stmt(jmp(Lab)) --> "br label %", atom(Lab).



llvm_stmt(_) --> [].







