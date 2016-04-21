:- module(optimize, [optimize/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(ordsets)).

:- use_module(utils).
:- use_module(optimize_blocks).

%%%%%%%%%%%%%%%%%%%%%%%%
% PROGRAM OPTIMIZATION %
%%%%%%%%%%%%%%%%%%%%%%%%

optimize(Prog, OptProg) :-
    phrase(divide(Strings, Functions, Decls), Prog),

    optimize_strings(Strings, OptStrings),
    maplist(optimize_function, Functions, OptFunctions),
    
    append([ Decls, OptStrings, OptFunctions], OptProg).


divide([], [], []) --> [].
divide([Str | Strs], Funs, Decls) -->
    [Str], { Str =.. [string | _] },
    divide(Strs, Funs, Decls).
divide(Strs, [Fun | Funs], Decls) -->
    [Fun], { Fun =.. [function | _] },
    divide(Strs, Funs, Decls).
divide(Strs, Funs, [Decl | Decls]) -->
    [Decl], { Decl =.. [decl | _] },
    divide(Strs, Funs, Decls).


optimize_strings([], []).
optimize_strings([H | T], TT) :- member(H, T), optimize_strings(T, TT).
optimize_strings([H | T], [H | TT]) :- optimize_strings(T, TT).

%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCTION OPTIMIZATION %
%%%%%%%%%%%%%%%%%%%%%%%%%

optimize_function(function(Type,Fun,Args,Body), function(Type,Fun,Args,OptBody)) :-
    blocks(Blocks0, Body, []),
    optimize_blocks(Blocks0, Blocks),
    blocks(Blocks, OptBody, []).








