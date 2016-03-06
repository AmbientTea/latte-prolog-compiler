:- module(llvm, []).

llvm_m(llvm{
    label: 1
}).
/*
M.next_label(M.label) := M.put(label, V) :- V is M.label + 1.

llvm(Prog, Out) :- llvm(1, Prog, Out).

llvm_s( fun(Ret, Fun, Args) ) -->
    "define ", term(Ret).
*/

llvm(X,X).
