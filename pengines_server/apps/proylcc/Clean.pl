:- module(clean, [chequear_y_limpiar_maximo/5]).
:- use_module(gravity).
:- use_module(fusion).
:- use_module(rango).
:- use_module(shoot).


% chequear_y_limpiar_maximo(+GridAntes, +GridDespues, +NumCols, -GridFinal, -Efectos)
chequear_y_limpiar_maximo(GridAntes, GridDespues, NumCols, GridFinal, Efectos) :-
    include(number, GridAntes, NumerosIniciales),
    include(number, GridDespues, NumerosFinales),
    ( NumerosIniciales \= [] -> max_list(NumerosIniciales, MaxInicial) ; MaxInicial = 0 ),
    ( NumerosFinales \= [] -> max_list(NumerosFinales, MaxFinal) ; MaxFinal = 0 ),

    ( range_for_max(MaxInicial, MinInicial, _),
      range_for_max(MaxFinal, MinFinal, MaxRFinal),
      MinInicial \= MinFinal ->
        limpiar_bloque(GridDespues, MinInicial, GridLimpio),
        apply_gravity_up_effects(GridLimpio, NumCols, GravityAfterCleanup),
        last(GravityAfterCleanup, effect(GridFinal, _)),

        % Notificación del nuevo rango
        Efectos = [effect(GridLimpio, [removed(MinInicial), range_changed(MinFinal, MaxRFinal)]) | GravityAfterCleanup]
    ;   GridFinal = GridDespues,
        Efectos = []
    ).



% Limpia todas las apariciones de un valor especifico
limpiar_bloque(GridIn, Valor, GridOut) :-
    maplist(reemplazar_si_igual(Valor), GridIn, GridOut).

reemplazar_si_igual(Valor, Valor, '-') :- !.
reemplazar_si_igual(_, Otro, Otro).
