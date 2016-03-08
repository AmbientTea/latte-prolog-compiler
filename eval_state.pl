:- module(eval_state, [new_eval_state/2]).

new_eval_state(Funs, eval_state{
    vars : [_{}],
    funs: Funs
}).


S.push() := S.put(vars, [_{} | S.vars]).
S.pop() := S.put(vars, Vars) :- S.vars = [_|Vars].

S.add_var(Id, Val) := S.put(vars, [Top.put(Id, Val) | Vars]) :- S.vars = [Top | Vars].
S.set_var(Id, Val) := S.put(vars, NewVars) :-
    append(Top, [E | Bot], S.vars),
    E.get(Id) = _, !,
    append(Top, [E.put(Id, Val) | Bot], NewVars).
    

S.get_var(Id) := Val :- member(Vars, S.vars), Val = Vars.get(Id), !.
