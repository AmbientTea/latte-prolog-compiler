:- module(optimize_blocks, [
    blocks//1,
    optimize_blocks/2,
    add//2
]).

:- use_module(library(dcg/basics)).
:- use_module(library(dialect/hprolog)).

:- use_module(peephole).
:- use_module(optimize_blocks).

%%%%%%%%%%%%%%
% BLOCK INFO %
%%%%%%%%%%%%%%

M.add(Set, Elem) := M.put(Set, NS) :- ord_add_element(M.Set, Elem, NS).

block_info(Blocks, Info) :-
    Blocks = [(StartBlock, _, _) | _],
    Info0 = optimize{
        phi_targets: [],
        jump_targets: [],
        jumps: [],
        start_block: StartBlock },
    get_block_info(Blocks, [Info0], [Info]).
    

% jump and phi information
get_block_info([]) --> [].
get_block_info([(Block, Body, End) | T]) -->
    ( { End = if(_, Target1, Target2) } ->
        do_state add(jump_targets, Target1),
        do_state add(jump_targets, Target2),
        do_state add(jumps, Block -> Target1),
        do_state add(jumps, Block -> Target2)
    ; { End = jump(Target) } ->
        do_state add(jumps, Block -> Target),
        do_state add(jump_targets, Target)
    ; [] ),
    get_block_info(Block, Body),
    get_block_info(T).

get_block_info(_, []) --> [].
get_block_info(Block, [ _ = phi(_, [(_, Lab1), (_, Lab2)]) | T ]) -->
    do_state add(phi_targets, Lab1 -> Block),
    do_state add(phi_targets, Lab2 -> Block),
    get_block_info(Block, T).
get_block_info(CurBlock, [_|T]) --> get_block_info(CurBlock, T).


% divide by blocks
blocks([]) --> [].
blocks([B|T]) --> block(B), blocks(T).

block((Label, Body, End)) -->
    [ block(Label) ],
    string(Body),
    block_end(End).

block_end(jump(Target)) --> [ jump(Target) ].
block_end(if(Cond, Then, Else)) --> [ if(Cond, Then, Else) ].
block_end(ret) --> [ ret, unreachable ].
block_end(ret) --> [ ret ].
block_end(ret(Type, V)) --> [ ret(Type, V) ].
block_end(ret(Type, V)) --> [ ret(Type, V), unreachable ].
block_end(unreachable) --> [ unreachable ].


%%%%%%%%%%%%%%%%%%%%%%
% BLOCK OPTIMIZATION %
%%%%%%%%%%%%%%%%%%%%%%

optimize_blocks(Blocks0, OptBlocks) :-
    block_info(Blocks0, BlockInfo),
    optimize_blocks(BlockInfo, Blocks0, Blocks1, []),
    ( Blocks0 \== Blocks1 ->
        optimize_blocks(Blocks1, OptBlocks)
    ; OptBlocks = Blocks1 ).

optimize_blocks(_, []) --> [].
optimize_blocks(Info, [H | T]) --> optimize_block(Info, H), optimize_blocks(Info, T).


% single block optimization
optimize_block(BlocksInfo, (Label, _, _)) -->
    { \+ memberchk_eq(Label, BlocksInfo.jump_targets),
      BlocksInfo.start_block \== Label }.

optimize_block(BlocksInfo, (Label1, [], jump(Label2))) -->
    { Label1 \== BlocksInfo.start_block, Label1 \== Label2,
      \+ memberchk_eq(Label1 -> Label2, BlocksInfo.phi_targets),
      Label1 = Label2 }.

optimize_block(BlocksInfo, (Label, Body, End)) -->
    { peephole(BlocksInfo.put(block, Label), Body, OptBody, []) },
    { End = if(If, Left, Right) ->
        ( If == 0 -> OptEnd = jump(Right)
        ; If == 1 -> OptEnd = jump(Left)
        ; End = OptEnd )
    ; End = OptEnd },
    
    [ (Label, OptBody, OptEnd) ].

optimize_block(_, Block) --> [Block].
