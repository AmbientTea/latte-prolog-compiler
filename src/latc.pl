#!/usr/bin/swipl -q

:- use_module(library(optparse)).

:- use_module(exception).

:- use_module(utils).
:- use_module(parser).
:- use_module(frontend).

:- use_module(compile).

:- initialization (
    catch( if_possible main
         , Exception
         , handle_exception(Exception) ),
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
	
	( Args = [File | _]       or_else throw(no_file) ),

	parse(File, Tree),

	check(_Cont, Tree, Env, NTree),

    compile(Opts, Env, NTree, Code),
    
    format(user_error, "OK~n", []),
    writeln(Code).

