:- module(exception, [
    handle_exception/1
]).

handle_exception(Ex) :-
    phrase(exception_message(Ex), Message),
    format(user_error, "ERROR~n~s~n", [Message]),
    halt(1).

exception_message(no_file) --> "no file specified".
exception_message(parsing_fail) --> "parsing failed".
exception_message(compilation_failed) --> "complation failed".
