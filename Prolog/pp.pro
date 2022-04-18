%%%%%%%%%%%% Motor de busqueda IDDFS %%%%%%%%%%%%%%% 
% NOTA: (TIENE LA POSIBILIDAD DE QUEDARSE EN BUCLE INFINITO CUANDO NO HAY SOLUCION POSIBLE )

% Hay que hacer simular()

iddfs(Problema, Estado, Movimientos) :-
    between(0,inf,Depth),
    ldfs(Problema, Estado, Movimientos, Depth).


ldfs(Problema, Estado, [], _) :-
    final(Problema, Estado).

ldfs(Problema, Estado, [Movimiento|Movimientos], Depth) :-
    Depth > 0,

    movimiento(Problema, Estado, Movimiento),
    moverse(Problema, Estado, Movimiento, Proximo),
    legal(Problema, Proximo),
    
    Depth0 is Depth - 1,

    ldfs(Problema, Proximo, Movimientos, Depth0).

resolver(Problema, Movimientos) :-
    inicial(Problema, Inicial),
    iddfs(Problema, Inicial, Movimientos).

%%%%%%%%%%%%%%%%%% Problema del caballero (Para probar el motor) %%%%%%%%%%%%

inicial(knight, knight(1, 1, 5, 10)).

final(knight, knight(5, 7, 5, 10)).

movimiento(knight, _, Move) :-
    member(Move, [[2,1], [1,2], [2,-1], [1,-2], [-1,-2], [-2, -1], [-2, 1], [-1, 2]]).

moverse(knight, knight(F, C, MaxF, MaxC), [DF, DC], knight(NF, NC, MaxF, MaxC)) :-
    NF is F + DF, NC is C + DC.

legal(knight, knight(F, C, MaxF, MaxC)) :-
    F >= 1, F =< MaxF,
    C >= 1, C =< MaxC.

:- discontiguous([inicial/2, final/2, movimiento/3, moverse/4, legal/2]).

%%%%%%%%%%%%%%    Problemas de los canales  %%%%%%%%%%%%%
% Los estados son el functor estado(Televisores, N, actual, last), donde Televisores son los canales
% actuales de cada televisor, N la cantidad de canales posibles, actual es el televisor al que se le
% cambio recietemente el canal y last es el televisor al que se le cambio el canal antes
% del ultimo televisor cambiado de canal

% No esta usando resolver()
% Hay que ver como hacer para que use resolver()

canales(Televisores, N, L) :-
    Inicial = estado(Televisores, N, -1, -1),
    iddfs(controlRemoto, Inicial, L).

final(controlRemoto, estado([X, X, X, X], _, _, _)).

movimiento(controlRemoto, _, Move) :-
    member(Move, [1,2,3,4]).

moverse(controlRemoto, estado(Televisores, N, NewLast, _), NewActual, estado(Televisores0, N, NewActual, NewLast)) :-
    nth1(NewActual, Televisores, Televisor),
    Televisor0 is (Televisor + 1) mod N,
    replace(Televisores, NewActual, Televisor0, Televisores0).

legal(controlRemoto, estado(_, _, X, Y)) :-
    X \= Y.

% Predicado auxiliar que cambia el elemento en la posicion Index 
% de una lista por el elemento E. Las posiciones se cuentan desde 1.
replace([_|R], 1, X, [X|R]).
replace([X|Xs], Index, E, [X|Ys]) :-
    Index0 is Index - 1,
    replace(Xs, Index0, E, Ys). 
    