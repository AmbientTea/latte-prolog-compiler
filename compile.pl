#!/usr/bin/prolog -q

:- use_module(library(optparse)).

:- use_module(utils).
:- use_module(parser).
:- use_module(frontend).
:- use_module(eval).

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
		  /*
		, [ opt(env)
		  , type(term)
		  , default(env{})
		  , shortflags([e])
		  , longflags(['env'])
		  , help(['specify environment'])
		  ]
		  */
    	],
	current_prolog_flag(argv, LineArgs),
	opt_parse(OptSpec, LineArgs, Opts, Args),
	member(content(Cont), Opts),
	
	(Args = [File | _]       -> true ; fail("no file specified")),
	(parse(Cont, File, Tree) -> true ; fail("parsing failed") ),
	(check(Tree)             /*-> true ; fail("type check failed")*/),
	( Cont = exp -> eval_exp(Tree)
	; Cont = stmt -> eval_stmt(Tree)
	; Cont = program -> eval_program(Tree) ),
	halt(0)
) ; halt(-1).

