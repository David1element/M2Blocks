:- module(fusion, [fusionar_si_igual/4, fusionar_una_vez/3]).

columns(5).
rows(7).

% UTILIDADES

index_to_coord(Index, Row, Col):- 
    columns(NumCols), 
    Row is Index // NumCols, 
    Col is Index mod NumCols.

coord_to_index(Row, Col, Index) :- 
    columns(NumCols), 
    Index is Row * NumCols + Col.

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :- 
    I > 0, 
    I1 is I - 1, 
    replace(T, I1, X, R).

replace_minus(Index, GridIn, GridOut) :-
    replace(GridIn, Index, -, GridOut).

coord_to_index_wrapper((R, C), Index) :-
    coord_to_index(R, C, Index).

% DFS PARA GRUPOS 

grupo_conectado(Grid, Index, Grupo) :-
    nth0(Index, Grid, Valor),
    Valor \= -,
    columns(_Cols), rows(_Rows),
    index_to_coord(Index, StartRow, StartCol),
    dfs(Grid, [(StartRow, StartCol)], Valor, [], IndicesCoords),
    maplist(coord_to_index_wrapper, IndicesCoords, Grupo).

dfs(_, [], _, Visitados, Visitados).
dfs(Grid, [(R,C)|Rest], Valor, Visitados, GrupoFinal) :-
    member((R,C), Visitados), !,
    dfs(Grid, Rest, Valor, Visitados, GrupoFinal).
dfs(Grid, [(R,C)|Rest], Valor, Visitados, GrupoFinal) :-
    coord_to_index(R, C, Index),
    nth0(Index, Grid, Valor),
    columns(Cols), rows(Rows),
    findall((NR,NC),
        (
            member((DR,DC), [(-1,0),(1,0),(0,-1),(0,1)]),
            NR is R + DR,
            NC is C + DC,
            NR >= 0, NR < Rows,
            NC >= 0, NC < Cols,
            coord_to_index(NR, NC, I2),
            nth0(I2, Grid, Valor),
            \+ member((NR,NC), Visitados)
        ),
        Vecinos),
    append(Rest, Vecinos, NuevaFrontera),
    dfs(Grid, NuevaFrontera, Valor, [(R,C)|Visitados], GrupoFinal).

% FUSION GRUPAL 

fusionar_grupo(Grid, Grupo, FusionIndex, NewGrid, ValorFusionado) :-
    nth0(FusionIndex, Grid, Valor),
    length(Grupo, N),
    ValorFusionado is Valor * (2 ** (N - 1)),
    delete(Grupo, FusionIndex, Rest),
    replace(Grid, FusionIndex, ValorFusionado, Temp),
    foldl(replace_minus, Rest, Temp, NewGrid).

% FUSION POR INDICE


fusionar_si_igual(Grid, Index, NewGrid, effect(NewGrid, [newBlock(ValorFusionado)])) :-
    grupo_conectado(Grid, Index, Grupo),
    length(Grupo, N), N > 1,
    ( member(Index, Grupo) ->
        FusionIndex = Index
    ;   Grupo = [FusionIndex|_] 
    ),
    fusionar_grupo(Grid, Grupo, FusionIndex, NewGrid, ValorFusionado).

fusionar_si_igual(Grid, _, Grid, effect(Grid, [])).

% FUSION GENERAL

fusionar_una_vez(Grid, FinalGrid, effect(FinalGrid, [newBlock(ValorFusionado)])) :-
    length(Grid, Len),
    fusionar_hasta_primera(Grid, 0, Len, FinalGrid, ValorFusionado),
    FinalGrid \= Grid, !.

fusionar_una_vez(Grid, Grid, effect(Grid, [])).

fusionar_hasta_primera(_Grid, I, Len, _NewGrid, _ValorFusionado) :-
    I >= Len, !, fail.

fusionar_hasta_primera(Grid, I, _Len, NewGrid, ValorFusionado) :-
    fusionar_si_igual(Grid, I, TempGrid, effect(TempGrid, [newBlock(ValorFusionado)])),
    TempGrid \= Grid, !,
    NewGrid = TempGrid.

fusionar_hasta_primera(Grid, I, Len, NewGrid, ValorFusionado) :-
    I1 is I + 1,
    fusionar_hasta_primera(Grid, I1, Len, NewGrid, ValorFusionado).

