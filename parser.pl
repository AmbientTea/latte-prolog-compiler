:- module(parser, [parse/2, tokenize//1]).
:- use_module(library(dcg/basics)).

keywords([ if, else, while, return, true, false, int, string, boolean, void]).
operators(["++", "--", "+", "-", "*", "/", "%", "(", ")", "{", "}", ";", "==", "!=",
            "=", "<=", "<", ">=", ">", "||", "&&", "!", ","]).

%op(600, xfy, !=).
%op(600, xfy, !=).

id(Id) --> [Start], { code_type(Start, alpha) }, id_cont(Cont), { atom_codes(Id, [Start | Cont]) }.
id_cont([H|T]) --> [H], { code_type(H, alnum) }, id_cont(T).
id_cont([]) --> "".

keyword(K) :- keywords(Keys), member(K, Keys).
operator(O) :- operators(Ops), member(O, Ops).

token(str(T)) --> "\""	, string(S), "\"", !, {atom_codes(T, S)}.
token(T) --> {operator(Tok)}, Tok, !, {atom_codes(T, Tok)}.
token(T) --> integer(T), !.
token(T) --> id(Id), { keywords(KS), (member(Id, KS) -> T = Id ; T = id(Id))}.


% comments
tokenize(Tok) --> "/*", !, comment1end, tokenize(Tok).
tokenize(Tok) --> "//", !, comment2end, tokenize(Tok).
tokenize(Tok) --> "#", !, comment2end, tokenize(Tok).

tokenize(X) --> blank, !, tokenize(X).
tokenize([Tok | Tail]) --> token(Tok), !, tokenize(Tail).
tokenize([]) --> [], !.

%tokenize(_), X --> { writeln(error), writeln(X), format("error at: ~s~n", [X]), fail }.


comment1end --> "*/", !.
comment1end --> [_], comment1end.

comment2end --> "\n", !.
comment2end --> [_], comment2end.



parse( File, Tree ) :-
	phrase_from_file(tokenize(Tokens), File),
	writeln(Tokens),
	( phrase(program(Tree), Tokens) ),
	writeln(Tree),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
program([Def|Defs]) --> topdef(Def), !, program(Defs).
program([]) --> [].

topdef(topdef(Type, Id, Args, Block)) --> type(Type), [id(Id), '('], args(Args), [')'], block(Block).


farg(arg(Type, Id)) --> type(Type), !, [id(Id)].
args([H|T]) --> farg(H), [,], !, args(T).
args([H]) --> farg(H).
args([]) --> [], !.

%%% LISTS
stmts([H|T]) --> stmt(H), !, stmts(T).
stmts([]) --> [], !.

inits([H|T]) --> init(H), [,], !, inits(T).
inits([H]) --> init(H).
inits([]) --> [].

%%% STATEMENTS %%%

block( Stmts ) --> ['{'], !, stmts(Stmts), ['}'], !.

init((Id, Exp)) --> [id(Id), =], !, exp(Exp).
init(Id) --> [id(Id)].

stmt( decl(Type, Ins) ) --> type(Type), inits(Ins), [;].

stmt( =(Id, Exp) ) --> [id(Id), =], exp(Exp), [;].

stmt(if(If, Then, Else)) --> [if, '('], exp(If), [')'], stmt(Then), [else], stmt(Else).
stmt(if(If, Then)) --> [if, '('], exp(If), [')'], stmt(Then).

stmt(while(While, Do)) --> [while, '('], exp(While), [')'], stmt(Do).

stmt(return(E)) --> [return], exp(E), [;], !.
stmt(return) --> [return], [;], !.

stmt(incr(Id)) --> [id(Id), '++', ;].
stmt(decr(Id)) --> [id(Id), '--', ;].

stmt(block(Stmts)) --> block(Stmts).

stmt(expstmt(Exp)) --> exp(Exp), [;].

% type
types([void, int, boolean, string]).
type(T) --> { types(Tps), member(T, Tps) }, [T].

% expressions
exps([H|T]) --> exp(H), [,], exps(T).
exps([E]) --> exp(E).
exps([]) --> [].

exp(E) --> orexp(E).


% simple
sexp(E) --> ['('], !, exp(E), [')'], !.
sexp(int(I)) --> [I], { integer(I) }, !.
sexp(int(I)) --> [-, IN], { integer(IN), I is -IN }, !.
sexp(str(S)) --> [str(S)], !.
sexp(app(Fun, Args)) --> [id(Fun), '('], !, exps(Args), [')'].
sexp(var(V)) --> [id(V)], !.
sexp(true) --> [true], !.
sexp(false) --> [false], !.

% logical
:- op(600, xfy, '&&').
:- op(600, xfy, '||').
orexp(E) --> andexp(E1), (['||'], !, orexp(E2), { E = '||'(E1,E2)} ; { E = E1 }).
andexp(E) --> lexp(E1), (['&&'], !, andexp(E2), { E = '&&'(E1,E2) } ; { E = E1 }).


% comp
:- op(600, xfy, '!=').
:- op(600, xfy, '<=').
:- op(600, xfy, '>=').

lexp(E) --> aexp(E).
lexp(not(E)) --> [!], !, lexp(E).


lexp(E) --> aexp(E1), [Op], { member(Op, [<,>,'<=','>=',==,'!=']) }, !, aexp(E2), { E =.. [Op, E1, E2] }.

% additive
aexp(E) --> mexp(E).
aexp(E) --> mexp(E1), [Op], { member(Op, [+,-]) }, !, aexp(E2), { E =.. [Op, E1, E2] }.

% multiplicative
mexp(E) --> sexp(E).
mexp(E) --> sexp(E1), [Op], { member(Op, [*,/,'%']) }, !, mexp(E2), { E =.. [Op, E1, E2] }.
