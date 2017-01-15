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

inst_topdef(string(Str, Lab, Len, Ind), C, C1) :-
    string_length(Str, Len),
    if_possible (Ind = 0),
    atomic_concat('@str', C, Lab), C1 is C+1.

inst_topdef(function(_, _, Args, Body), C, C) :-
    foldl(inst_arg, Args, 1, _),
    foldl(inst_instr, Body, (0,1), _).

inst_topdef(_Def, C, C).

inst_arg((V,_), C, C1) :- atomic_concat('%arg', C, V), C1 is C + 1.    

inst_instr(V = _, (C,LC), (C1,LC)) :-
    atomic_concat('%', C, V), C1 is C + 1.
inst_instr(block(Bl), (C,LC), (C,LC1)) :-
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
type(string) --> "i8*".
type(boolean) --> "i1".
type(void) --> "void".
type(class(Type)) --> "%", atom(Type).
type(ref(Type)) --> type(Type), "*".
type(array(Type)) --> type(Type), "*".

types(Types) --> separated(", ", type, Types).

% arguments
fun_arg((Var, Type)) --> type(Type), " ", atom(Var).
args(Args) --> separated(", ", fun_arg, Args).

phi_arg((V, Lab)) --> "[", atom(V), ", %", atom(Lab), "]".

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

topdef(string(Str, Lab, Len, 0)) -->
    atom(Lab), " = private constant [", atom(Len), " x i8] c\"",
    { atom_codes(Str, Codes) }, llvm_string(Codes), "\", align 1\n".

topdef(class(Name, Fields)) -->
    "%", atom(Name), " = type {", separated(", ", type, Fields), "}\n". 

% suffix substring
topdef(string(_Str, _Lab, _Len, _Ind)) --> [].

indent(block(_)) --> "".
indent(_) --> "    ".

stmts([]) --> [].
stmts([H|T]) --> "\n", indent(H), stmt(H), !, stmts(T).

%%%%%%%%%%%%%%%%%%
%%% STATEMENTS %%%
%%%%%%%%%%%%%%%%%%

stmt(block(B)) --> atom(B), ":".

stmt(V = Right ) --> atom(V), " = ", rightval(Right).

stmt(call(Fun, Args)) -->
    "call void @", atom(Fun), "(", args(Args), ")".

stmt(if(Cond, Lab1, Lab2)) -->
    "br i1 ", atom(Cond), ", label %", atom(Lab1), ", label %", atom(Lab2).

stmt(jump(Lab)) --> "br label %", atom(Lab).

stmt(ret) --> "ret void".
stmt(ret(Type, V)) --> "ret ", type(Type), " ", atom(V).

stmt(unreachable) --> "unreachable".

stmt(store(Type, Ptr, Val)) -->
    "store ", type(Type), " ", atom(Val), ", ", type(Type), "* ", atom(Ptr).

stmt(S) --> "*unrecognized*: ", atom(S).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASSIGNMENT VALUES %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

rightval(null) --> "null".
rightval(phi(Type, Args)) -->
    "phi ", type(Type), " ", separated(", ", phi_arg, Args).

rightval(call(Type, Fun, Args)) -->
    "call ", type(Type), " @", atom(Fun), "(", args(Args), ")".

%%% STRING CONSTANTS %%%
rightval(strcast(Len, Lab, 0)) -->
    "bitcast [", atom(Len), " x i8]* ", atom(Lab), " to i8*".

% complex constant expression to access a suffix of a string
%0 = getelementptr i8, i8* bitcast ([8 x i8]* @str0 to i8*), i32 1
rightval(strcast(Len, Lab, Ind)) -->
    "getelementptr i8, i8* bitcast ([", atom(Len), " x i8]* ", atom(Lab), " to i8*), i32 ", atom(Ind).

%%% POINTER HANDLING %%%
rightval(cast(V, From, To)) -->
    "bitcast ", type(From), " ", atom(V), " to ", type(To).

rightval(getmemberptr(ref(Class), Ptr, Pos)) -->
    "getelementptr ", type(Class), ", ", type(Class), "* ", atom(Ptr), ", i32 0, i32 ", atom(Pos).

rightval(getelemptr(Type, Arr, Ind)) -->
    "getelementptr ", type(Type), ", ", type(array(Type)), " ", atom(Arr), ", i32 ", atom(Ind).
    
rightval(load(Type, Reg)) -->
    "load ", type(Type), ", ", type(ref(Type)), " ", atom(Reg).

%%% OPERATORS %%%
rightval(OpE) -->
    { OpE =.. [Op, V1, V2], operator(Op, LLOp, InT, _) },
    LLOp, " ", InT, " ", atom(V1), ", ", atom(V2).

rightval('=='(Type, V1, V2)) -->
    "icmp eq ", type(Type), " ", atom(V1), ", ", atom(V2).

rightval('!='(Type, V1, V2)) -->
    "icmp ne ", type(Type), " ", atom(V1), ", ", atom(V2).

