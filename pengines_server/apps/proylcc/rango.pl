:- module(rango, [
    range_for_max/3,
    is_power_of_two/1,
    opciones_validas/2
]).

% Devuelve el rango valido [Low, High] según el maximo valor en la grilla
range_for_max(Max, 2, 4)     :- member(Max, [2, 4, 8]).
range_for_max(16, 2, 8).
range_for_max(32, 2, 16).
range_for_max(64, 2, 32).
range_for_max(Max, 2, 64)    :- member(Max, [128, 256, 512]).
range_for_max(1024, 4, 128).
range_for_max(2048, 8, 256).
range_for_max(Max, 16, 512)  :- member(Max, [4096, 8192]).
range_for_max(16384, 32, 1024).

% Extension generica para valores mayores a 16384
range_for_max(Max, Low, High) :-
    Max > 16384,
    log2(Max, Exp),
    LowExp is Exp - 9,
    HighExp is Exp - 4,
    Low is 2 ** LowExp, 
    High is 2 ** HighExp.

log2(X, Exp) :-
    X > 0,
    Exp is floor(log(X) / log(2)).  % log base 2 usando log natural

is_power_of_two(N) :-
    integer(N),
    N > 0,
    0 is N /\ (N - 1).

% Lista de bloques validos entre Low y High que sean potencias de dos
opciones_validas(0, [2, 4]) :- !.
opciones_validas(Max, Opciones) :-
    range_for_max(Max, Low, High),
    findall(X, (
        between(Low, High, X),
        is_power_of_two(X)
    ), Opciones).