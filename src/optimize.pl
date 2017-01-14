:- module(optimize, [optimize/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(ordsets)).

:- use_module(utils).
:- use_module(optimize_blocks).

%%%%%%%%%%%%%%%%%%%%%%%%
% PROGRAM OPTIMIZATION %
%%%%%%%%%%%%%%%%%%%%%%%%

optimize(Prog, OptProg) :-
    phrase(divide(Strings, Classes, Functions, Decls), Prog),

    optimize_strings(Strings, OptStrings),
    maplist(optimize_function, Functions, OptFunctions),
    
    append([ Decls, Classes, OptStrings, OptFunctions], OptProg).

% divide(?Strings, ?Classes, ?Functions, ?FunDecls)
% <-->
% Strings ++ Classes ++ Functions ++ FunDecls
divide([], [], [], []) --> [].
divide([Str | Strs], Clss, Funs, Decls) -->
    [Str], { Str =.. [string | _] },
    divide(Strs, Clss, Funs, Decls).
divide(Strs, [Class | Clss], Funs, Decls) -->
    [Class], { Class =.. [class | _] },
    divide(Strs, Clss, Funs, Decls).
divide(Strs, Clss, [Fun | Funs], Decls) -->
    [Fun], { Fun =.. [function | _] },
    divide(Strs, Clss, Funs, Decls).
divide(Strs, Clss, Funs, [Decl | Decls]) -->
    [Decl], { Decl =.. [decl | _] },
    divide(Strs, Clss, Funs, Decls).


optimize_strings(Strs, OptStrs) :-
    merge_dup_strings(Strs, NoDupStrs),
    % reverse all strings
    maplist(reverse_str, NoDupStrs, RevStrs),
    % sort them, so each has its suffixes immediately to the right
    sort(1, '@>', RevStrs, SortedStrs),
    % optimize out prefixes
    prefixes(SortedStrs, Prefs),
    maplist(reverse_str_back, OptStrs, Prefs)
.

% merge duplicate string constants
merge_dup_strings([], []).
merge_dup_strings([H | T], TT) :- member(H, T), merge_dup_strings(T, TT).
merge_dup_strings([H | T], [H | TT]) :- merge_dup_strings(T, TT).

% suffix optimization
reverse_str( string(Str, Lab, Len, Ind), string(RevCodes, Lab, Len, Ind) ) :-
    string_codes(Str, Codes),
    reverse(Codes, RevCodes).

reverse_str_back( string(Str, Lab, Len, Ind), string(RevCodes, Lab, Len, Ind) ) :-
    reverse(Codes, RevCodes),
    string_codes(Str, Codes).

% if Str2 is a suffix of Str1, sets Ind2 to non-zero value so it gets accessed
% by indexing Str1. Sorting the list ensures Str2 to prefix either Str1 or none.
prefixes([ string(Str1, Lab1, Len1, Ind1), string(Str2, Lab2, Len2, Ind2) | T ], TT )  :-
    append(Str2, Pref, Str1),
    length(Pref, Ind2),
    Lab2 = Lab1, Len2 = Len1,
    prefixes([string(Str1, Lab1, Len1, Ind1) | T], TT).

prefixes([H | T], [H | TT]) :- prefixes(T, TT).
prefixes([], []).
%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCTION OPTIMIZATION %
%%%%%%%%%%%%%%%%%%%%%%%%%

optimize_function(function(Type,Fun,Args,Body),
                  function(Type,Fun,Args,OptBody)) :-
    blocks(Blocks0, Body, []),
    optimize_blocks(Blocks0, Blocks),
    blocks(Blocks, OptBody, []).








