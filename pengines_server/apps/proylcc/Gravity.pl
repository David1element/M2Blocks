:- module(gravity, [apply_gravity_up_effects/3, replace/4, check_values_with_empty_above/3, apply_moves/4]).

% Reemplaza un valor en la lista
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

check_values_with_empty_above(Grid, NumCols, Result) :-
    length(Grid, _Len),
    findall(Index,
        (
            nth0(Index, Grid, Value),
            Value \= '-',                        % No esta vacio
            Index >= NumCols,                   % Hay una fila arriba
            AboveIndex is Index - NumCols,
            nth0(AboveIndex, Grid, AboveValue),
            AboveValue == '-'                   % La celda de arriba esta vacia
        ),
        Result).

% Reemplaza un valor por - y mueve ese valor hacia arriba
move_up(Grid, Index, NumCols, NewGrid) :-
    nth0(Index, Grid, Value),
    AboveIndex is Index - NumCols,
    replace(Grid, AboveIndex, Value, TempGrid),
    replace(TempGrid, Index, '-', NewGrid).

% Aplicamos el movimiento hacia arriba en todos los elementos de la lista
apply_moves(Grid, [], _, Grid). 

apply_moves(Grid, [Index|Rest], NumCols, FinalGrid) :-
    move_up(Grid, Index, NumCols, UpdatedGrid),
    apply_moves(UpdatedGrid, Rest, NumCols, FinalGrid).


% Gravedad hacia arriba con efectos
apply_gravity_up_effects(Grid, NumCols, [effect(Grid, [])|RestEffects]) :-
    check_values_with_empty_above(Grid, NumCols, Indices),
    Indices \= [],
    apply_moves(Grid, Indices, NumCols, TempGrid),
    apply_gravity_up_effects(TempGrid, NumCols, RestEffects).

apply_gravity_up_effects(Grid, _, [effect(Grid, [])]). % Caso base