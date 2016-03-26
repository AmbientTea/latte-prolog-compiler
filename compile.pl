:- module(compile, [compile/3]).

:- use_module(ir2).

:- use_module(llvm).
:- use_module(peephole).

compile(Env, Prog, Out) :-
    ir_program(Env,Prog, IR), !,
    optimize(IR, OpIR), !,
    llvm:compile(OpIR, Out1),
    string_chars(Out, Out1)
.

%%%%%%%%%%%5

optimize(X,X).
/*
optimize([], []).
optimize( [fun(Type,Fun,Args,Body) | T],
          [fun(Type,Fun,Args,OptBody) | OptT] ) :-
    phrase(peephole(Body), OptBody),
    optimize(T, OptT).
*/
