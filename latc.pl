#!/usr/bin/prolog -q

:- use_module(library(optparse)).

:- use_module(utils).
:- use_module(parser).
:- use_module(frontend).
:- use_module(eval).

:- use_module(compile).


:- % main
(
	OptSpec =
		[ [ opt(content)
		  , type(atom)
		  , default(program)
		  , shortflags([t])
		  , longflags(['content'] )
		  , help([ 'parsed content: exp, prog'])
		  ]
		, [ opt(mode)
		  , type(atom)
		  , default(eval)
		  , shortflags([m])
		  , longflags(['mode'])
		  , help(['mode: eval, compile'])
		  ]
		, [ opt(out)
		  , type(string)
		  , default("")
		  , shortflags([o])
		  , longflags(['out'])
		  , help(['output file (compilation only)'])
		  ]
    	],
	current_prolog_flag(argv, LineArgs),
	opt_parse(OptSpec, LineArgs, Opts, Args),
	member(content(Cont), Opts),
	member(mode(Mode), Opts),
	
	(Args = [File | _]       -> true ; fail("no file specified")),
	( member(out(Out), Opts), Out \= ""
	; file_name_extension(Base, _, File), file_name_extension(Base, ".out", Out)),
	(parse(Cont, File, Tree) -> true ; fail("parsing failed") ),
	(check(Tree, Env)             /*-> true ; fail("type check failed")*/),
	( Mode = eval ->
	    ( Cont = exp -> eval_exp(Tree)
	    ; Cont = stmt -> eval_stmt(Tree)
	    ; Cont = program -> eval_program(Tree) )
    ; Mode = compile ->
        ( compile(llvm, Tree, Code),
            writeln(Code)
        ; fail("compilation not implemented yet"))
    ; fail("invalid mode: ~w", [Mode])),
	halt(0)
) ; /* writeln("something went wrong"), */ halt(-1).

