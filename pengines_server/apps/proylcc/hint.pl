:- module(hint, [hint/5]).
:- use_module(shoot).

hint(Block, Column, Grid, NumCols, hint(Column, Block, TotalValue, NumBlocks, MaxValue)) :-
    shoot(Block, Column, Grid, NumCols, Effects),
    count_fusions(Effects, NumBlocks, TotalValue, MaxValue).

count_fusions(Effects, NumBlocks, TotalValue, MaxValue) :-
    findall(Value,
      (
        member(effect(_, Infos), Effects),
        member(newBlock(Value), Infos)
      ),
      Values),
    length(Values, NumBlocks),
    sum_list(Values, TotalValue),
    max_list(Values, MaxValue).
