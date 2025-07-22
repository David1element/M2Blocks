:- module(proylcc, 
	[  
		randomBlock/2,
		shoot/5,
		hint/5
	]).
:- use_module(init, [init/2]).
:- use_module(shoot).
:- use_module(hint).
:- use_module(rango).


randomBlock(Grid, Block) :-
    include(number, Grid, OnlyNumbers),
    ( OnlyNumbers == [] -> Max = 0 ; max_list(OnlyNumbers, Max) ),
    opciones_validas(Max, Opciones),
    random_member(Block, Opciones).
	

% Reemplaza el valor en una posicion de la lista
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).


