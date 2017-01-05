:- module(compile, [compile/4]).

:- use_module(ir2).

:- use_module(llvm).
:- use_module(optimize).

compile(Opts, Env, Prog, Out) :-
    program(Env,Prog, IR), !,
    (member(optimize(true), Opts) ->
        % writeln(user_error, optimizing),
        optimize(IR, OpIR)
    ; IR = OpIR),
    llvm:compile(OpIR, Out1),
    string_chars(Out, Out1)
.

