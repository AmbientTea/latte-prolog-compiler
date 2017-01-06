#!/usr/bin/prolog -q

:- use_module(library(optparse)).

:- use_module(utils).
:- use_module(parser).
:- use_module(frontend).

:- use_module(compile).

:- initialization (
    catch( if_possible main
         , Exception
         , (print_message(error, Exception), halt(-1)) ),
    halt(0)
).

command_line_arguments(Opts, Args) :-
	OptSpec =
		[ [ opt(out)
		  , type(string)
		  , default("")
		  , shortflags([o])
		  , longflags(['out'])
		  , help(['output file'])
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
	opt_parse(OptSpec, LineArgs, Opts, Args).

main :-
    command_line_arguments(Opts, Args),
	
	(Args = [File | _]       or_else fail("no file specified")),

	(parse(Cont, File, Tree) or_else fail("parsing failed") ),

	check(Cont, Tree, Env, NTree),

    ( compile(Opts, Env, NTree, Code)
    or_else fail("compilation error or some features not implemented yet")),
    
    writeln(Code).

