:- module(eval, [eval_exp/1, eval_stmt/1, eval_program/1]).

:- use_module(eval_m).

eval_exp(exp(Exp)) :-
    eval_m(M),
    writeln( eval : M.eval_exp(Exp) ).

eval_stmt(stmt(Stmt)) :-
    eval_m(M),
    _ = M.eval_stmt(Stmt),
    writeln(done).

eval_program(program(Prog)) :-
    eval_m(EM),
    M = EM.load_program(Prog),
    _ = M.eval_function(main, []).get_return().
