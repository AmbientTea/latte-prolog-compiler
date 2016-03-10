:- module(llvm, [llvm_compile/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(apply)).

% top level llvm translation
llvm_compile(In, Out) :-
    llvm_inst(In),
    phrase(llvm_compile(In), Out).

%%% instantiation %%%

llvm_inst(Prog) :- maplist(llvm_inst_fun, Prog).

llvm_inst_fun(fun(_, _, Args, Body)) :-
    foldl(llvm_inst_arg, Args, 1, _),
    foldl(llvm_inst_instr, Body, (1,1), _).

llvm_inst_arg((V,_), C, C1) :- atomic_concat('%arg', C, V), C1 is C + 1.    

llvm_inst_instr(V = _, (C,LC), (C1,LC)) :- atomic_concat('%', C, V), C1 is C + 1.
llvm_inst_instr(block(Bl), (C,LC), (C,LC1)) :-
    atomic_concat('label', LC, Bl),
    LC1 is LC + 1.
llvm_inst_instr(_, C, C).

%%%%%%%%%%%%%%%%%%%
%%% translation %%%
%%%%%%%%%%%%%%%%%%%
llvm_compile([]) --> [].
llvm_compile([H|T]) --> llvm_fun(H), llvm_compile(T).


% types
llvm_type(string) --> "i8*".
llvm_type(int) --> "i32".
llvm_type(boolean) --> "i1".
llvm_type(void) --> "void".

% arguments
llvm_args([]) --> [].
llvm_args([(Var,Type) | T]) -->
    llvm_type(Type), " ", atom(Var),
    ( { T = [] } -> [] ; ", ", llvm_argS(T)).

% operator info
llvm_op(+, "add", "i32", "i32").
llvm_op(-, "sub", "i32", "i32").
llvm_op(*, "mul", "i32", "i32").
llvm_op(/, "div", "i32", "i32").

llvm_op(>, "icmp sgt", "i32", "i1").


% functions
llvm_fun(fun(Type, Fun, Args, Body)) -->
    "define ", llvm_type(Type), " @", atom(Fun), "(", llvm_args(Args), "){",
    llvm_stmts(Body),
    "\n}". 


indent(block(_)) --> "".
indent(_) --> "    ".
llvm_stmts([]) --> [].
llvm_stmts([H|T]) --> "\n", indent(H), /* atom(H), " ----> ", */ llvm_stmt(H), !, llvm_stmts(T).


% statements
llvm_phi_args([]) --> [].
llvm_phi_args([(V,Lab) | T]) -->
    "[", atom(V.reg), ", %", atom(Lab), "]", ({ T = []} -> [] ; ", ", llvm_phi_args(T)).

% llvm_stmt(S) --> { writeln(left:S), fail }.

llvm_stmt(block(B)) --> atom(B), ":".

llvm_stmt(  V3 = phi(Type, Args) ) -->
    atom(V3), " = phi ", llvm_type(Type), " ", llvm_phi_args(Args).

llvm_stmt(V = call(Type, Fun, Args)) -->
    atom(V), " = call ", llvm_type(Type), " @", atom(Fun), "(", llvm_args(Args), ")".
llvm_stmt(call(Fun, Args)) -->
    "call ", llvm_type(void), " @", atom(Fun), "(", llvm_args(Args), ")".

llvm_stmt(V = OpE) -->
    { OpE =.. [Op, V1, V2], llvm_op(Op, LLOp, InT, _) },
    atom(V), " = ", LLOp, " ", InT, " ", atom(V1), ", ", atom(V2).

llvm_stmt(if(Cond, Lab1, Lab2)) -->
    "br i1 ", atom(Cond), ", label %", atom(Lab1), ", label %", atom(Lab2).

llvm_stmt(jmp(Lab)) --> "br label %", atom(Lab).

llvm_stmt(ret) --> "ret void".
llvm_stmt(ret(Type, V)) --> "ret ", llvm_type(Type), " ", atom(V).

llvm_stmt(_) --> [].







