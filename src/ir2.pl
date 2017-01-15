/* Program compilation to itermediate representation that closely resembles
 * LLVM assembly. All register, argument and label identifiers are represented
 * by logical variables with unification used to connect dependencies and
 * substitute constants.
 */

:- module(ir2, [program/3]).

:- use_module(library(lists)).

:- use_module(utils).

:- op(600, xfy, ++).
:- op(600, xfy, '&&').
:- op(600, xfy, '||').

% each construction when compiled returns all its dependencies to the outside
% to allow for compilation to SSA form independent from local environment. Only
% global one is passed around.
% sets need uniform e tag for unification
empty_deps(ir2{
    % All three sets follow the format: Variable: Type - Register
    % Register may hold a register variable or a constant
    
    % Variables undefined within the construction
    ask: e{},
    % Variables modified within the construction with new register variables
    % holding the results
    mod: e{},
    % Variables defined within construction with their initial value
    gen: e{},
    
    % Some constructions need to know the initial or final blocks of others or
    % themselves. 
    block_in: Block,
    block_out: Block
}) --> [].

ask_dep(Id, Reg, Dep) -->
    empty_deps(Dep1),
    { Dep = Dep1.put(ask/Id, Reg) }.

ask_dep(Ask, Dep) -->
    empty_deps(Dep1),
    { Dep = Dep1.put(ask, Ask) }.

%%%%%%%%%%%%%%%
%%% MERGING %%%
%%%%%%%%%%%%%%%

% [e1 + e2] merges dependencies of two subexpressions of an expression
expression_merge(Dep1, Dep2, Dep) --> 
    empty_deps(EmptyDep),
    {
        Ask set_is Dep1.ask + Dep2.ask,
        Dep = EmptyDep.put(ask, Ask)
    }.

% [i1 ; i2] merges environments of two consecutive instructions
semicolon_merge( Dep1, Dep2, Dep ) --> {
    Dep = ir2{
        ask: Ask,
        mod: Mod,
        gen: Gen,
        block_in: Dep1.block_in,
        block_out: Dep2.block_out
    },
    Dep1.block_out = Dep2.block_in,
    Ask set_is Dep1.ask + (Dep2.ask ~ Dep1.gen ~ Dep1.mod),
    Mod set_is Dep2.mod + (Dep1.mod - Dep2.mod) - Dep1.gen,
    Gen set_is Dep1.gen + Dep2.gen
}.

% [i1 || i2] merges environments of two alternatives in an IF statement
or_merge(Dep1, Dep2, Dep) --> {
    Dep = ir2{
        ask: Ask,
        mod: Mod,
        gen: e{},
        block_in: _BlockIn,
        block_out: _BlockOut
        },
    Ask set_is Dep1.ask + Dep2.ask,
    union(Dep1.mod.keys(), Dep2.mod.keys(), ModKeys)
    },
    
    dcg_map(or_merge_phi(Dep1, Dep2), ModKeys, NewRegs),
    
    { dict_pairs(Mod, e, NewRegs) }.

or_merge_phi(Dep1, Dep2, Key, Key - (Type - Reg)) -->
    [ Reg = phi(Type, [(V1, Dep1.block_out), (V2, Dep2.block_out)]) ],
    {
        (Type - V1 = Dep1.mod.get(Key), ! ; Type - V1 = Dep1.ask.get(Key)),
        (Type - V2 = Dep2.mod.get(Key), ! ; Type - V2 = Dep2.ask.get(Key))
    }.

% [e * i] merges environments of the instruction and condition of WHILE statement
while_merge(BlockIn, WhileDep, DoDep, Dep) --> {
    Dep = ir2{
        ask: Ask,
        mod: Mod,
        gen: e{},
        block_in: BlockIn,
        block_out: WhileDep.block_out
        },
    maplist(fst(-), M, DoDep.mod.keys()),
    dict_pairs(D, _, M), % create brand new registers for keys in DoDep.mod
    Ask set_is WhileDep.ask - DoDep.mod + D + (DoDep.ask - DoDep.mod)
    },
    
    dcg_map(while_merge_phi(BlockIn, Ask, DoDep), DoDep.mod.keys(), NewRegs),
    
    {
        dict_pairs(Mod, e, NewRegs),
        WhileDep.ask >:< Mod,
        DoDep.ask >:< Mod
    }
.

while_merge_phi(StartBlock, WhileAsk, DoDep, Key, Key - (Type - Reg)) -->
    [ Reg = phi(Type, [(V1, StartBlock), (V2, DoDep.block_out)]) ],
    {
        Type - V1 = WhileAsk.Key,
        Type - V2 = DoDep.mod.Key
    }.

%%%%%%%%%%%%%%%%%%%
%%% ir %%%
%%%%%%%%%%%%%%%%%%%

% EXPRESSIONS

exps(_Env, [], [], Dep) --> empty_deps(Dep).
exps(Env, [Exp | L], [Reg | LL], Dep) -->
    exp(Env, Exp, Reg, Dep1),
    exps(Env, L, LL, Dep2),
    expression_merge(Dep1, Dep2, Dep).
    
% exp(+Environment, +Expression, -Result Value, -Dependencies)
exp(_Env, null, null, Dep) -->
    empty_deps(Dep).
exp(_Env, int(I), I, Dep) -->
    empty_deps(Dep).
exp(_Env, false, 0, Dep) -->
    empty_deps(Dep).
exp(_Env, true, 1, Dep) -->
    empty_deps(Dep).

exp(Env, cast(From, To, Exp), Reg, Dep) -->
    { format(user_error, "~w~n", [From - To - Exp]) },
    exp(Env, Exp, ExpReg, Dep),
    [ Reg = cast(ExpReg, From, To) ].
    

exp(Env, str(Str), V, Dep) -->
    { member(Str - StrLab - Len - Index, Env.strings) },
    [ V = strcast(Len, StrLab, Index) ],
    empty_deps(Dep).

exp(_Env, var(VarType, Id), Reg, Dep) -->
    ask_dep(Id, VarType - Reg, Dep).

exp(Env, new(Type), Reg, Dep) -->
    empty_deps(Dep),
    malloc(Env, class(Type), 1, Reg).

exp(Env, new_arr(Type, LenExp), Reg, Dep) -->
    exp(Env, LenExp, LenV, Dep),
    malloc(Env, Type, LenV, Reg).

exp(Env, LeftExp, Reg, Dep) -->
    leftval(Env, LeftExp, LeftVal, Dep),
    load(LeftVal, Reg).

exp(Env, app(Fun, ArgExps), V, Dep) -->
    exps(Env, ArgExps, ArgVals, Dep),
    {
        zip(ArgVals, Env.functions.Fun.args, Args),
        Type = Env.functions.Fun.return
    },
    ({ Type = void } ->
      [ call(Fun, Args) ]
    ; [ V = call(Type, Fun, Args) ]).


exp(Env, E1 ++ E2, V, Dep) -->
    exp(Env, app(concat, [E1, E2]), V, Dep).

exp(Env, E, V, Dep) -->
    { E =.. [Op, Type, E1, E2], member(Op, ['!=', '==']), VV =.. [Op, Type, V1, V2] }, !,
    exp(Env, E1, V1, Dep1),
    exp(Env, E2, V2, Dep2),
    [V = VV],
    expression_merge(Dep1, Dep2, Dep).

exp(Env, E, V, Dep) -->
    { E =.. [Op, E1, E2], member(Op, [+,-,*,/,'%',<,>,'<=','>=']), VV =.. [Op, V1, V2] }, !,
    exp(Env, E1, V1, Dep1),
    exp(Env, E2, V2, Dep2),
    [V = VV],
    expression_merge(Dep1, Dep2, Dep).

exp(Env, Exp, V, Dep) -->
    % guard for looping
    { member(Exp, [not(_), _ '||' _, _ && _]) }, 
    cond(Env, Exp, True, False, Dep1),
    { Dep = Dep1.put(block_out, End) },
    
    [ block(True),
      jump(End) ],
    
    [ block(False),
      jump(End) ],
    
    [ block(End),
      V = phi(boolean, [(1, True), (0, False)]) ].

% boolean expressions

% cond(+Environment, +BoolExpression, ?JumpToIfTrue, ?JumpToIfFalse, -Dependencies)
cond(Env, not(Exp), LabTrue, LabFalse, Dep) -->
    cond(Env, Exp, LabFalse, LabTrue, Dep).


cond(Env, E1 && E2, LabTrue, LabFalse, Dep) -->
    cond(Env, E1, Second, LabFalse, Dep1),
    
    [ block(Second) ],
    cond(Env, E2, LabTrue, LabFalse, Dep2),
    { Dep2.block_in = Second },
    
    expression_merge(Dep1, Dep2, Dep3),
    { Dep = Dep3.put(block_out, _BlockOut) }.

cond(Env, E1 '||' E2, LabTrue, LabFalse, Dep) -->
    cond(Env, E1, LabTrue, Second, Dep1),
    
    [ block(Second) ],
    cond(Env, E2, LabTrue, LabFalse, Dep2),
    { Dep2.block_in = Second },

    expression_merge(Dep1, Dep2, Dep3),
    { Dep = Dep3.put(block_out, _BlockOut) }.


cond(_Env, var(VarType, Id), LabTrue, LabFalse, Dep) -->
    ask_dep(Id, VarType - Reg, Dep),
    [ if(Reg, LabTrue, LabFalse) ].

cond(Env, Exp, LabTrue, LabFalse, Dep) -->
    exp(Env, Exp, V, Dep),
    [ if(V, LabTrue, LabFalse) ].

%%%%%%%%%%%%%%%
% LEFT VALUES %
%%%%%%%%%%%%%%%

leftval(_Env, var(Type, Id), reg(Reg), Dep) -->
    empty_deps(Empty),
    { Dep = Empty.put(mod/Id, Type - Reg) }.

leftval(Env, field(Class, Exp, Field), ptr(Type, Ptr), Dep) -->
    exp(Env, Exp, Obj, Dep),
    { nth0(Pos, Env.classes.Class.fields, Field - Type) },
    [ Ptr = getmemberptr(ref(class(Class)), Obj, Pos) ].

% allocate Len objects of type Class
malloc(Env, Type, Len, Reg) -->
    { Size is Len * Env.type_size(Type) },
    [ Reg1 = call(string, malloc, [(Size, int)]) ],
    [ Reg = cast(Reg1, string, ref(Type)) ].


store(reg(Reg), Val) --> { Reg = Val }.
store(ptr(Type, Ptr), Val) --> [ store(Type, Ptr, Val) ].

load(ptr(Type, Ptr), Reg) -->
    [ Reg = load(Type, Ptr) ].
load(reg(Reg), Reg) --> [].
    
    

%%%%%%%%%%%%%%%%%%
%%% STATEMENTS %%%
%%%%%%%%%%%%%%%%%%

stmts(_ConsEnv, [], Dep) --> empty_deps(Dep).
stmts(Env, [Stmt | Stmts], Dep) -->
    stmt(Env, Stmt, Dep1),
    stmts(Env, Stmts, Dep2),
    semicolon_merge(Dep1, Dep2, Dep).

% stmt(+Environment, +Statement, -Dependencies)
stmt(_Env, skip, Dep) --> empty_deps(Dep).

stmt(Env, block(Stmts), Dep) -->
    stmts(Env, Stmts, StmtDep),
    { Dep = StmtDep.put(gen, e{}) }.

stmt(Env, LeftVal = Exp, Dep) -->
    exp(Env, Exp, Val, ExpDep),
    leftval(Env, LeftVal, Target, LeftDep),
    store(Target, Val),
    semicolon_merge(ExpDep, LeftDep, Dep).

stmt(Env, (Id : VarType) = Exp, Dep) -->
    exp(Env, Exp, V, ExpDep),
    { Dep = ExpDep.put(mod/Id, VarType - V) }.

stmt(_Env, return, Dep) --> [ret], empty_deps(Dep).
stmt(Env, return(RetType, Exp), Dep) -->
    exp(Env, Exp, V, Dep),
    [ret(RetType, V)].


stmt(_Env, decl(_Type, []), Dep) --> empty_deps(Dep).

stmt(Env, decl(Type, [ init(Id, Exp) | T ]), Dep) -->
    exp(Env, Exp, V, ExpDep),
    stmt(Env, decl(Type,T), StmtDep),
    semicolon_merge(ExpDep.put(gen/Id, Type - V), StmtDep, Dep).

stmt(Env, expstmt(Exp), Dep) -->
    exp(Env, Exp, _, Dep).


stmt(Env, if(If, Then, Else), Dep) -->
    cond(Env, If, ThenBlock, ElseBlock, IfDep),
    
    [ block(ThenBlock) ],
    stmt(Env, Then, ThenDep),
    { ThenDep.block_in = ThenBlock },
    [ jump(EndBlock) ],
    
    [ block(ElseBlock) ],
    stmt(Env, Else, ElseDep),
    { ElseDep.block_in = ElseBlock },
    [ jump(EndBlock) ],
    
    [ block(EndBlock) ],
    
    or_merge(ThenDep, ElseDep, OrEnv),
    semicolon_merge(IfDep, OrEnv, Dep1),
    { Dep = Dep1.put(block_out, EndBlock) }.
    

stmt(Env, while(While, Do), Dep) -->
    [ jump(WhileBlock) ],
    
    [ block(WhileBlock) ],
    leave_gap(MergeGap),
    cond(Env, While, DoBlock, EndBlock, WhileDep),
    { WhileDep.block_in = WhileBlock },
    
    [ block(DoBlock) ],
    stmt(Env, Do, DoDep),
    { DoDep.block_in = DoBlock },
    [ jump(WhileBlock) ],
    
    [ block(EndBlock) ],
    
    fill_gap(MergeGap, while_merge(BlockIn, WhileDep, DoDep, Dep1)),
    
    { Dep = Dep1.put(block_in, BlockIn).put(block_out, EndBlock) }
. % while

%%%%%%%%%%%%%%%
%%% CLASSES %%%
%%%%%%%%%%%%%%%
class_definition(_Env, class_def(_Name, _Fields, _Methods)) --> [].

class_declaration(Name - Info) -->
    { maplist(snd(-), Info.fields, FieldTypes) },
    [ class(Name, FieldTypes) ].

%%%%%%%%%%%%%%%%%
%%% FUNCTIONS %%%
%%%%%%%%%%%%%%%%%

function_body(Env, Body, Dep) -->
    [ block(StartBlock) ],
    stmts(Env, Body, Dep),
    { Dep.block_in = StartBlock },
    % last block can be empty due to returns in bramches.
    [ unreachable ].

% creates register variables for each argument and creates MOD set from them
fun_args([], e{}, []).
fun_args([(Id,Type) | T], SS, [(Reg,Type) | TT]) :-
    fun_args(T, S, TT),
    SS = S.put(Id,Type - Reg).


function_definition(Env, fun_def(Ret, Fun, Args, Body)) -->
    % { format(user_error, "compiling function: ~w : ~w -> ~w~n", [Fun, Args, Ret]) },
    {
        fun_args(Args, Mod, NArgs),
        phrase(function_body(Env, Body, Dep), Code),
        % plug in the arguments
        Mod >:< Dep.ask
    },
    [ function(Ret, Fun, NArgs, Code) ].

function_declaration(_Fun - FunInfo) -->
    { FunInfo.extern = false, ! }.
function_declaration(Fun - FunInfo) -->
    [ decl(Fun, FunInfo.return, FunInfo.args) ].

%%%%%%%%%%%%%%%
%%% PROGRAM %%%
%%%%%%%%%%%%%%%

toplevel_definition(Env, Def) -->
    ( function_definition(Env, Def) ; class_definition(Env, Def) ).


string_declaration(Str1 - Lab - Len - Ind) -->
    { string_concat(Str1, "\0", Str) },
    % we include blank index variable for possible suffix optimization
    [ string(Str, Lab, Len, Ind) ].

program(Env, Program) -->
    { dict_pairs(Env.classes, _Tag, ClassPairs) },
    dcg_map(class_declaration, ClassPairs),
    dcg_map(function_declaration, Env.functions),
    dcg_map(string_declaration, Env.strings),
    dcg_map(toplevel_definition(Env), Program).


program(Env, Program, IR) :-
    phrase(program(Env, Program), IR), !
.




