:- module(exception, [
    handle_exception/1
]).

:- use_module(library(dcg/basics)).

handle_exception(Ex) :-
    phrase(exception_message(Ex), Message),
    format(user_error, "ERROR~n~s~n", [Message]),
    halt(1).

exception_message(no_file) --> "no file specified".
exception_message(compilation_failed) --> "complation failed".


% PARSER
exception_message(tokenize_fail(Line)) -->
    "unrecognized token at line ", atom(Line).
exception_message(unclosed_comment(Line)) -->
    "unclosed multiline comment starting at line ", atom(Line).
exception_message(unopened_comment(Line)) -->
    "unmatched multiline comment close at ", atom(Line).
exception_message(stdin_read_fail) -->
    "reading from stdin failed (note: lazy reading not yet supported)".
exception_message(parsing_fail) --> "parsing failed".

% FRONTEND
% expressions
exception_message(not_declared(Var)) -->
    "variable ", atom(Var), " not declared".
exception_message(bad_args(Fun, ExpectedTypes, ArgTypes)) -->
    "function ", atom(Fun), " takes arguments of types ", atom(ExpectedTypes),
    " but got ", atom(ArgTypes).
exception_message(no_function(Fun)) -->
    "function ", atom(Fun), " does not exist".
exception_message(bad_type(Expr, ExpectedType, Type)) -->
    "expression ", atom(Expr), " has type ", atom(Type), " but type ",
    atom(ExpectedType), " was expected".

% statements
exception_message(dupl_decl(Id)) -->
    "variable ", atom(Id), " already declared in its scope".
exception_message(bad_return(Ret, ExpectedRet, Exp)) -->
    "return of type ", atom(ExpectedRet), " expected but expression ", atom(Exp),
    " of type ", atom(Ret), " found".
exception_message(bad_void_return(ExpectedRet)) -->
    "return of type ", atom(ExpectedRet), " expected but void return found".
exception_message(bad_increment(Id, Type)) -->
    "cannot increment variable ", atom(Id), " of type ", atom(Type).
exception_message(bad_increment(Id, Type)) -->
    "cannot decrement variable ", atom(Id), " of type ", atom(Type).

% program
exception_message(no_return(Fun)) -->
    "control flow reaches function ", atom(Fun), " end without return".
exception_message(dupl_arg(Arg)) -->
    "argument ", atom(Arg), " declared more than once".