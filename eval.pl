:- module(eval, [eval_exp/1, eval_stmt/1, eval_program/1]).

:- use_module(eval_m).

eval_exp(Exp) :-
    eval_m(M),
    writeln( eval : M.eval_exp(Exp) ).

eval_stmt(Stmt) :-
    eval_m(M),
    M ? eval_stmt(Stmt),
    writeln(done).

eval_program(Prog) :-
    eval_m(EM),
    M = EM.load_program(Prog),
    M ? eval_function(main, []).get_return().
