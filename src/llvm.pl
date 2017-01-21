:- module(llvm, [compile/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(utils).

% top level llvm translation
compile(In, Out) :-
    inst(In),
    phrase(compile(In), Out).

%%%%%%%%%%%%%%%%%%%%%
%%% instantiation %%%
%%%%%%%%%%%%%%%%%%%%%

inst(Prog) :- foldl(inst_topdef, Prog, 0, _).

inst_topdef(string(Str, glob(Lab), Len, Ind), C, C1) :-
    string_length(Str, Len),
    if_possible (Ind = 0),
    atomic_concat('str', C, Lab), C1 is C+1.

inst_topdef(function(_, _, Args, Body), C, C) :-
    foldl(inst_arg, Args, 1, _),
    foldl(inst_instr, Body, (0,1), _).

inst_topdef(_Def, C, C).

inst_arg((V,_), C, C1) :- atomic_concat('%arg', C, V), C1 is C + 1.    

inst_instr(reg(C) = _, (C,LC), (C1,LC)) :- C1 is C + 1.
inst_instr(block(block(Bl)), (C,LC), (C,LC1)) :-
    atomic_concat('label', LC, Bl), LC1 is LC + 1.
inst_instr(ret(_,_), (X,C), (X1,C)) :- X1 is X+1.
inst_instr(ret, (X,C), (X1,C)) :- X1 is X+1.
inst_instr(_, C, C).

%%%%%%%%%%%%%%%%%%%
%%% translation %%%
%%%%%%%%%%%%%%%%%%%
compile(Prog) --> dcg_map(topdef, Prog).

% types
type(int) --> "i32".
type(char) --> "i8".
type(string) --> "i8*".
type(boolean) --> "i1".
type(void) --> "void".
type(struct(Types)) --> "{", separated(", ", type, Types), "}".
type(class(Type)) --> "%", atom(Type).
type(ref(Type)) --> type(Type), "*".
type(array(Type)) --> "{ i32, ", type(ref(Type)), "}".
type(function(RetType, ArgTypes)) -->
    type(RetType), " (", separated(", ", type, ArgTypes), ")*".
type(vector(Len, Type)) --> "[", atom(Len), " x ", type(Type), "]".

types(Types) --> separated(", ", type, Types).

value(reg(Reg)) --> "%", atom(Reg).
value(glob(Glob)) --> "@", atom(Glob).
value(block(Block)) --> "%", atom(Block).
value(V) --> atom(V).

% arguments
argument((Var, Type)) --> type(Type), " ", value(Var).
args(Args) --> separated(", ", argument, Args).

phi_arg((V, Block)) --> "[", value(V), ", ", value(Block), "]".

% strings
llvm_string([C|T]) -->
    { atom_codes(A, [C]) },
    (
        % LLVM uses ASCII hex codes as escape values
          { A = '\n' } -> "\\0A"
        ; { A = '\t' } -> "\\09"
        ; { A = '\e' } -> "\\1B"
        ; { A = '\a' } -> "\\07"
        ; { A = '\"' } -> "\\22"
        ; { A = '\\' } -> "\\5C"
        ; { A = '\0' } -> "\\00"
        ;  [C]
    ), llvm_string(T).
llvm_string([]) --> [].

% operator info
operator(+, "add", "i32", "i32").
operator(-, "sub", "i32", "i32").
operator(*, "mul", "i32", "i32").
operator(/, "sdiv", "i32", "i32").
operator('%', "srem", "i32", "i32").

operator(>, "icmp sgt", "i32", "i1").
operator(<, "icmp slt", "i32", "i1").
operator(>, "icmp sgt", "i32", "i1").
operator('<=', "icmp sle", "i32", "i1").
operator('>=', "icmp sge", "i32", "i1").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TOP LEVEL DEFINITIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

topdef(function(Type, Fun, Args, Body)) -->
    "define ", type(Type), " @", atom(Fun), "(", args(Args), "){",
    stmts(Body),
    "\n}\n". 
topdef(decl(Fun, Type, Args)) -->
    "declare ", type(Type), " @", atom(Fun), "(", types(Args), ")\n".

topdef(string(Str, Block, Len, 0)) -->
    value(Block), " = private constant [", atom(Len), " x i8] c\"",
    { atom_codes(Str, Codes) }, llvm_string(Codes), "\", align 1\n".

topdef(class(Class, Info, Fields, Methods)) -->
    "; class ", atom(Class), "\n",
    type(Info.vtable_type_label), " = type ", type(Info.vtable_type), "\n",
    "%", atom(Class), " = type ", type(struct(Fields)), "\n",
    "@", atom(Info.vtable_label), " = constant ", type(Info.vtable_type_label), "{", args(Methods), "}\n\n". 

% suffix substring
topdef(string(_Str, _Lab, _Len, _Ind)) --> [].

topdef(Top) --> "*unrecognized*: ", atom(Top), "\n".


indent(block(_)) --> "".
indent(_) --> "    ".

stmts([]) --> [].
stmts([H|T]) --> "\n", indent(H), stmt(H), !, stmts(T).

%%%%%%%%%%%%%%%%%%
%%% STATEMENTS %%%
%%%%%%%%%%%%%%%%%%

stmt(block(block(B))) --> atom(B), ":".

stmt(V = Right ) --> value(V), " = ", rightval(Right).

stmt(call(Fun, Args)) -->
    "call void ", value(Fun), "(", args(Args), ")".

stmt(if(Cond, Lab1, Lab2)) -->
    "br i1 ", value(Cond), ", label ", value(Lab1), ", label ", value(Lab2).

stmt(jump(Block)) --> "br label ", value(Block).

stmt(ret) --> "ret void".
stmt(ret(Type, V)) --> "ret ", type(Type), " ", value(V).

stmt(unreachable) --> "unreachable".

stmt(store(Type, Ptr, Val)) -->
    "store ", type(Type), " ", value(Val), ", ", type(Type), "* ", value(Ptr).

stmt(S) --> "*unrecognized*: ", atom(S).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASSIGNMENT VALUES %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

rightval(null) --> "null".
rightval(phi(Type, Args)) -->
    "phi ", type(Type), " ", separated(", ", phi_arg, Args).

rightval(call(Type, Fun, Args)) -->
    "call ", type(Type), " ", value(Fun), "(", args(Args), ")".

%%% STRING CONSTANTS %%%
rightval(strcast(Len, Block, 0)) -->
    "bitcast ", type(ref(vector(Len, char))), " ", value(Block), " to i8*".

% complex constant expression to access a suffix of a string
%0 = getelementptr i8, i8* bitcast ([8 x i8]* @str0 to i8*), i32 1
rightval(strcast(Len, Block, Ind)) -->
    "getelementptr i8, i8* bitcast (", type(ref(vector(Len, char))), " ", value(Block), " to i8*), i32 ", atom(Ind).

%%% POINTER HANDLING %%%
rightval(cast(V, From, To)) -->
    "bitcast ", type(From), " ", value(V), " to ", type(To).

rightval(getptr(Type, Ptr, Inds)) -->
    "getelementptr ", type(Type), ", ", type(ref(Type)), " ", value(Ptr), ", i32 ",
        separated(", i32 ", value, Inds).

rightval(load(Type, Reg)) -->
    "load ", type(Type), ", ", type(ref(Type)), " ", value(Reg).

%%% OPERATORS %%%
rightval(OpE) -->
    { OpE =.. [Op, V1, V2], operator(Op, LLOp, InT, _) },
    LLOp, " ", InT, " ", value(V1), ", ", value(V2).

rightval('=='(Type, V1, V2)) -->
    "icmp eq ", type(Type), " ", value(V1), ", ", value(V2).

rightval('!='(Type, V1, V2)) -->
    "icmp ne ", type(Type), " ", value(V1), ", ", value(V2).

