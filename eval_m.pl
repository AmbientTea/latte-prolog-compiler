:- module(eval_m, [eval_m/1]).

:- use_module(utils).
:- use_module(eval_state).

eval_m(eval_m{
    vars: [_{}],
    funs: _{}
}).

M.new() := M.put(vars, [_{}]).

M.push() := M.put(vars, [_{} | M.vars]).
M.pop() := M.put(vars, Vars) :- M.vars = [_|Vars].

M.add_var(Id, Val) := M.put(vars, [Top.put(Id, Val) | Vars]) :- M.vars = [Top | Vars].
M.set_var(Id, Val) := M.put(vars, NewVars) :-
    append(Top, [Mid | Bot], M.vars),
    Mid.get(Id) = _, !,
    append(Top, [Mid.put(Id, Val) | Bot], NewVars).

M.get_var(Id) := Val :- member(Vars, M.vars), Val = Vars.get(Id), !.

M.get_return() := Ret :-
    Ret = M.get(return) ; Ret = ... .

%%%%%%%%%%%%%%%%%%%
%%% EXPRESSIONS %%%
%%%%%%%%%%%%%%%%%%%

M.eval_exps([H|T]) := [M.eval_exp(H) | M.eval_exps(T)].
_.eval_exps([]) := [].

_.eval_exp(int(I)) := I :- integer(I).
_.eval_exp(str(S)) := S :- string(S).
_.eval_exp(true) := true.
_.eval_exp(false) := false.

M.eval_exp(var(Id)) := M.get_var(Id).

M.eval_exp(app(printString, [Exp])) := _ :- writeln(M.eval_exp(Exp)), !.
M.eval_exp(app(printInt, [Exp])) := _ :- writeln(M.eval_exp(Exp)), !.
M.eval_exp(app(printBool, [Exp])) := _ :- writeln(M.eval_exp(Exp)), !.
M.eval_exp(app(Fun, ArgExps)) := M.new().eval_function(Fun, ArgVals).get_return() :-
    ArgVals = M.eval_exps(ArgExps).


M.eval_exp(E1 + E2) := V :- 
    V1 = M.eval_exp(E1),
    V2 = M.eval_exp(E2),
    V is V1 + V2.
M.eval_exp('++'(E1,E2)) := V :-
    V1 = M.eval_exp(E1),
    V2 = M.eval_exp(E2),
    string_concat(V1, V2, V). 
M.eval_exp(E1 - E2) := V :- V is M.eval_exp(E1) - M.eval_exp(E2).
M.eval_exp(E1 * E2) := V :- V is M.eval_exp(E1) * M.eval_exp(E2).
M.eval_exp(E1 / E2) := V :- V is M.eval_exp(E1) div M.eval_exp(E2).
M.eval_exp('%'(E1,E2)) := V :- V is M.eval_exp(E1) mod M.eval_exp(E2).

M.eval_exp(E1 < E2) := V :- M.eval_exp(E1) < M.eval_exp(E2) -> V = true ; V = false.
M.eval_exp(E1 > E2) := V :- M.eval_exp(E1) =< M.eval_exp(E2) -> V = false ; V = true.
M.eval_exp('=='(_,E1,E2)) := V :- M.eval_exp(E1) = M.eval_exp(E2) -> V = true ; V = false.
M.eval_exp('<='(E1,E2)) := V :- M.eval_exp(E1) =< M.eval_exp(E2) -> V = true ; V = false.
M.eval_exp('>='(E1,E2)) := V :- M.eval_exp(E1) >= M.eval_exp(E2) -> V = true ; V = false.
M.eval_exp('!='(_, E1,E2)) := V :- M.eval_exp(E1) = M.eval_exp(E2) -> V = false ; V = true.

M.eval_exp(not(Exp)) := V :- M.eval_exp(Exp) = true -> V = false ; V = true.
M.eval_exp('||'(E1,E2)) := V :-
      M.eval_exp(E1) = true -> V = true
    ; M.eval_exp(E2) = true -> V = true
    ; V = false.
M.eval_exp('&&'(E1,E2)) := V :-
    M.eval_exp(E1) = true, M.eval_exp(E2) = true -> V = true ; V = false.
% _.eval_exp(Exp) := _ :- fail("cannot eval_exp: ~w", [Exp]).

%%%%%%%%%%%%%%%%%%
%%% STATEMENTS %%%
%%%%%%%%%%%%%%%%%%

M.eval_stmts(_) := M :- _ = M.get(return), !.
M.eval_stmts([]) := M.
M.eval_stmts([H|T]) := M.eval_stmt(H).eval_stmts(T).

% _.eval_stmt(S) := _ :- writeln('running' : S), fail.

M.eval_stmt(_) := M :- _ = M.get(return), !.

M.eval_stmt(skip) := M.
M.eval_stmt(return) := M.put(return, ...).

M.eval_stmt(return(Exp)) := M.put(return, M.eval_exp(Exp)).

M.eval_stmt(incr(Id)) := M.set_var(Id, V) :- !,V is M.get_var(Id) + 1.
M.eval_stmt(decr(Id)) := M.set_var(Id, V) :- !,V is M.get_var(Id) - 1.

M.eval_stmt(Id = Exp) := M.set_var(Id, M.eval_exp(Exp)).

M.eval_stmt(block(Stmts)) := M.push().eval_stmts(Stmts).pop() :- !.

M.eval_stmt(while(While, Do)) := M.push().eval_stmt(Do).pop().eval_stmt(while(While,Do)) :-
    M.eval_exp(While) = true, !.
M.eval_stmt(while(_, _)) := M.

M.eval_stmt(if(If, Then, _)) := M.push().eval_stmt(Then).pop() :- M.eval_exp(If) = true, !.
M.eval_stmt(if(_, _, Else)) := M.push().eval_stmt(Else).pop() :- !.
M.eval_stmt(if(If, Then)) := M.eval_stmt(if(If, Then, skip)).

M.eval_stmt(decl(_, [])) := M :- !.
M.eval_stmt(decl(Type, [H|T])) := M.add_var(Id, Val).eval_stmt(decl(_, T)) :-
    H = noinit(Id) -> (
          Type = int -> Val = 0
        ; Type = boolean -> Val = false
        ; Type = string -> Val = "")
    ;
    H = init(Id, Exp) -> Val = M.eval_exp(Exp), !.

M.eval_stmt(expstmt(Exp)) := M :- !, _ = M.eval_exp(Exp).


%%%%%%%%%%%%%%%
%%% PROGRAM %%%
%%%%%%%%%%%%%%%

arg_names([], []).
arg_names([(Nm, _) | T], [Nm | TT]) :- arg_names(T, TT).

M.load_program([]) := M.
M.load_program([topdef(_Ret, Fun, Args, Body) | Tail]) :=
    M.put(funs, M.funs.put(Fun, (ArgNames, Body))).load_program(Tail) :-
        arg_names(Args, ArgNames).

M.eval_function(Fun, Args) := M.put(vars,[V]).eval_stmt(block(Body)) :-
    % format("running ~w with args: ~w~n", [Fun, Args]),
    (ArgNames, Body) = M.funs.Fun,
    pairs_keys_values(Pairs, ArgNames, Args),
    dict_create(V, _, Pairs).
