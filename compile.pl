:- module(compile, [compile/3]).

:- use_module(eval_state).
:- use_module(ir).

:- use_module(llvm).

compile(llvm, exp(Exp), Out) :-
    phrase(ir_exp(_, Exp, _), Out).

compile(llvm, stmt(Stmt), Out)  :-
    new_eval_state(St),
    phrase(ir_stmt(St, Stmt, _), Out).

compile(llvm, program(Prog), Out) :-
    phrase(ir_program(Prog), IR),
    llvm(IR, Out).

