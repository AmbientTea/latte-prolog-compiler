#!/usr/bin/env swipl

:- use_module(library(dialect/sicstus/system)).
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
	
	( Args = [File | _] or_else throw(no_file) ),
	( file_exists(File) or_else throw(file_missing(File))),

	parse(File, Tree),

	check(Tree, Env, NTree),

    compile(Opts, Env, NTree, Code),
    
    format(user_error, "OK~n", []),
    writeln(Code).

