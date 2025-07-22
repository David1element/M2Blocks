:- module(shoot, [shoot/5]).
:- use_module(gravity).
:- use_module(fusion).
:- use_module(rango).
:- use_module(clean).

 /*
    Build Prolog query, which will be something like:
    shoot(2, 2, [4,2,8,64,32,2,-,-,4,16,-,-,-,-,2,-,-,-,-,16,-,-,-,-,2,-,-,-,-,-,-,-,-,-,-], 5, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block).
*/

% fusionar_y_aplicar_gravedad(+Grid, -FinalGrid, -EffectsAcc, -AllEffects)
fusionar_y_aplicar_gravedad(Grid, NumCols, FinalGrid, EffectsAcc, AllEffects) :-
    fusionar_una_vez(Grid, FusedGrid, effect(FusedGrid, FusionEffects)),
    FusedGrid \= Grid,
    apply_gravity_up_effects(FusedGrid, NumCols, GravityEffects),
    last(GravityEffects, effect(LastGrid, _)),
    append(EffectsAcc, [effect(FusedGrid, FusionEffects) | GravityEffects], NewAcc),
    fusionar_y_aplicar_gravedad(LastGrid, NumCols, FinalGrid, NewAcc, AllEffects).

fusionar_y_aplicar_gravedad(Grid, _NumCols, Grid, Effects, Effects). 

fusionar_con_gravedad(Grid, NumCols, FinalGrid, AllEffects) :-
    fusionar_y_aplicar_gravedad(Grid, NumCols, FinalGrid, [], AllEffects).



shoot(Block, Column, Grid, NumCols, Effects) :-
    place_block(Block, Column, Grid, NumCols, PlacedGrid),
    apply_gravity_up_effects(PlacedGrid, NumCols, GravityEffects),
    last(GravityEffects, effect(GridAfterGravity, _)),
    fusionar_con_gravedad(GridAfterGravity, NumCols, GridAfterFusion, FusionEffects),
    append(GravityEffects, FusionEffects, EfectosPrevios),
    chequear_y_limpiar_maximo(Grid, GridAfterFusion, NumCols, GridFinal, EfectosLimpieza),
    append(EfectosPrevios, EfectosLimpieza, Effects1),
    fusionar_con_gravedad(GridFinal, NumCols, _FinalGrid, FusionEffects2),
    append(Effects1, FusionEffects2, Effects).






place_block(Block, Column, Grid, NumCols, NewGrid) :-
    ColIndex is Column - 1,
    length(Grid, Len),
    NumRows is Len // NumCols,
    find_first_empty_row(NumRows, ColIndex, NumCols, Grid, TargetIndex),
    ( TargetIndex \= -1 ->
        replace_nth0(TargetIndex, Grid, Block, NewGrid)
    ; NewGrid = Grid  % si no hay lugar, se deja igual
    ).

% Encuentra el indice de la primera celda vacia desde abajo en la columna
find_first_empty_row(NumRows, ColIndex, NumCols, Grid, Index) :-
    MaxRow is NumRows - 1,  % <-- esto evalúa la resta
    findall(I, (
        between(0, MaxRow, Offset),
        Row is MaxRow - Offset,
        I is Row * NumCols + ColIndex,
        nth0(I, Grid, Cell),
        Cell == '-'
    ), [Index|_]), !.
find_first_empty_row(_, _, _, _, -1).



% Reemplaza el valor en un indice de la lista
replace_nth0(Index, List, Value, NewList) :-
    same_length(List, NewList),
    append(Prefix, [_|Suffix], List),
    length(Prefix, Index),
    append(Prefix, [Value|Suffix], NewList).

