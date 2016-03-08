:- module(compile, [compile/3]).

:- use_module(eval_state).
:- use_module(ir).

:- use_module(llvm).
:- use_module(peephole).

compile(llvm, exp(Exp), Out) :-
    phrase(ir_exp(_, Exp, _), Out).

compile(llvm, stmt(Stmt), Out)  :-
    new_eval_state(St, _),
    phrase(ir_stmt(St, Stmt, _), Out).

compile(llvm, program(Prog), Out) :-
    ir_program(Prog, IR),
    writeln(IR),
    optimize(IR, OpIR),
    llvm_compile(OpIR, Out1),
    string_chars(Out, Out1).


%%%%%%%%%%%5

optimize(X,X).
optimize([], []).
optimize( [fun(Type,Fun,Args,Body) | T],
          [fun(Type,Fun,Args,OptBody) | OptT] ) :-
    phrase(peephole(Body), OptBody),
    optimize(T, OptT).
