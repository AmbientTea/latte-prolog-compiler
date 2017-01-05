#!/usr/bin/prolog -q

:- use_module(library(optparse)).

:- use_module(utils).
:- use_module(parser).
:- use_module(frontend).

:- use_module(compile).


:- % main
(
	OptSpec =
		[ [ opt(out)
		  , type(string)
		  , default("")
		  , shortflags([o])
		  , longflags(['out'])
		  , help(['output file (compilation only)'])
		  ]
		, [ opt(optimize)
		  , type(atom)
		  , default(true)
		  , shortflags(['O'])
		  , longflags(['optimize'])
		  , help(['optimize: true, false'])
		  ]
    	],
	current_prolog_flag(argv, LineArgs),
	opt_parse(OptSpec, LineArgs, Opts, Args),

	(Args = [File | _]       -> true ; fail("no file specified")),
	( member(out(Out), Opts), Out \= ""
	; file_name_extension(Base, _, File), file_name_extension(Base, ".out", Out)),

	(parse(Cont, File, Tree) -> true ; fail("parsing failed") ),

	(check(Cont, Tree, Env, NTree) /*-> true ; fail("type check failed")*/),

    ( compile(Opts, Env, NTree, Code), writeln(Code)
    ; fail("compilation error or some features not implemented yet")),
	
	halt(0)
) ; /* writeln("something went wrong"), */ halt(-1).

