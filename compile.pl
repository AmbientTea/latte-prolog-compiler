:- module(compile, [compile/3]).

:- use_module(eval_state).
:- use_module(ir).

:- use_module(llvm).
:- use_module(peephole).

compile(_, exp(Exp), Out) :-
    phrase(ir_exp(_, Exp, _), Out).

compile(Env, stmt(Stmt), Out)  :-
    new_eval_state(Env, St),
    phrase(ir_stmt(St, Stmt, _), Out).

compile(Env, program(Prog), Out) :-
    ir_program(Env,Prog, IR),
    % writeln(IR),
    optimize(IR, OpIR),
    /*
    writeln(OpIR), writeln("\n\n\n"),
    term_variables(OpIR, Vars), writeln(xXX:Vars),
    maplist(random_between(1,99), Vars),
    */
    llvm_compile(OpIR, Out1),
    string_chars(Out, Out1).


%%%%%%%%%%%5

optimize(X,X).
/*
optimize([], []).
optimize( [fun(Type,Fun,Args,Body) | T],
          [fun(Type,Fun,Args,OptBody) | OptT] ) :-
    phrase(peephole(Body), OptBody),
    optimize(T, OptT).
*/
