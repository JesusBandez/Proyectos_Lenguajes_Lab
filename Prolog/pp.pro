%%%%%%%%%%%% Motor de busqueda IDDFS %%%%%%%%%%%%%%% 
% NOTA: (TIENE LA POSIBILIDAD DE QUEDARSE EN BUCLE INFINITO CUANDO NO HAY SOLUCION POSIBLE )

% Hay que hacer simular()

iddfs(Problema, Inicial, Movimientos) :-
    iddfs(Problema, Inicial, [Inicial], Movimientos).

iddfs(Problema, Estado, Historia, Movimientos) :-
    between(0,inf,Depth),
    ldfs(Problema, Estado, Historia, Movimientos, Depth).


ldfs(Problema, Estado, _, [], _) :-
    final(Problema, Estado).

ldfs(Problema, Estado, Historia, [Movimiento|Movimientos], Depth) :-
    Depth > 0,

    movimiento(Problema, Estado, Movimiento),
    moverse(Problema, Estado, Movimiento, Proximo),
    legal(Problema, Proximo),
    \+ member(Proximo, Historia),

    Depth0 is Depth - 1,

    ldfs(Problema, Proximo, [Proximo|Historia], Movimientos, Depth0).

resolver(Problema, Movimientos) :-
    inicial(Problema, Inicial),
    iddfs(Problema, Inicial, Movimientos).

%%%%%%%%%%%%%%%%%% Problema del caballero (Para probar el motor) %%%%%%%%%%%%

%inicial(knight, knight(1, 1, 5, 10)).
%
%final(knight, knight(5, 7, 5, 10)).
%
%movimiento(knight, _, Move) :-
%    member(Move, [[2,1], [1,2], [2,-1], [1,-2], [-1,-2], [-2, -1], [-2, 1], [-1, 2]]).
%
%moverse(knight, knight(F, C, MaxF, MaxC), [DF, DC], knight(NF, NC, MaxF, MaxC)) :-
%    NF is F + DF, NC is C + DC.
%
%legal(knight, knight(F, C, MaxF, MaxC)) :-
%    F >= 1, F =< MaxF,
%    C >= 1, C =< MaxC.

:- discontiguous([inicial/2, final/2, movimiento/3, moverse/4, legal/2]).
:- dynamic inicial/2.
:- dynamic final/2.
%%%%%%%%%%%%%%    Problemas de los canales  %%%%%%%%%%%%%
% Los estados son el functor estado(Televisores, N, actual, last), donde Televisores son los canales
% actuales de cada televisor, N la cantidad de canales posibles, actual es el televisor al que se le
% cambio recietemente el canal y last es el televisor al que se le cambio el canal antes
% del ultimo televisor cambiado de canal


%canales(Televisores, N, L) :-
%    Inicial = estado(Televisores, N, -1, -1),
%    iddfs(controlRemoto, Inicial, L).

canales(Televisores, N, L) :-
    asserta(inicial(controlRemoto, estado(Televisores, N, -1, -1))),
    resolver(controlRemoto, L),
    retractall(inicial(controlRemoto, _)).

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
    
%%%%%%%%% Problema de patio de operaciones %%%%%%%%%%
% Los estados se representan como un fuctor: estado(Base, Arriba, Abajo) donde Base es 
% es la entrada a la "Y", Arriba es el brazo superior y Abajo el brazo inferior. Siendo cada
% uno una lista.

vagones(CuelloInicial, CuelloFinal, Operaciones) :-
    asserta(inicial(vagones, estado(CuelloInicial, [], []))),
    asserta(final(vagones, estado(CuelloFinal, [], []))),
    resolver(vagones, Operaciones),
    retractall(final(vagones, _)),
    retractall(inicial(vagones, _)).

% Todos los movimientos posibles de push
movimiento(vagones, estado([Vagon|R], _, _), push(Direccion, VagonesAMover)) :-
    length([Vagon|R], Len),
    between(1, Len, VagonesAMover),
    direccion(Direccion).

direccion(above).
direccion(below).
% Todos los movimientos de pop desde Above
movimiento(vagones, estado(_, [Vagon|R], _), pop(above, VagonesAMover)) :-
    length([Vagon|R], Len),
    between(1, Len, VagonesAMover).


% Todos los movimientos de pop desde Below
movimiento(vagones, estado(_, _, [Vagon|R]), pop(below, VagonesAMover)) :-
    length([Vagon|R], Len),
    between(1, Len, VagonesAMover).


% Moverse empujando los vagones hacia el brazo de arriba
moverse(vagones, estado(Base, Arriba, Abajo), push(above, VagonesAMover), estado(BaseNew, ArribaNew, Abajo)) :-
    length(L, VagonesAMover),
    append(BaseNew, L, Base),
    append(L, Arriba, ArribaNew).

% Moverse empujando los vagones hacia el brazo de abajo
moverse(vagones, estado(Base, Arriba, Abajo), push(below, VagonesAMover), estado(BaseNew, Arriba, AbajoNew)) :-
    length(L, VagonesAMover),
    append(BaseNew, L, Base),
    append(L, Abajo, AbajoNew).
    
% Moverse sacando los vagones de arriba
moverse(vagones, estado(Base, Arriba, Abajo), pop(above, VagonesAMover), estado(BaseNew, ArribaNew, Abajo)) :-
    length(L, VagonesAMover),
    append(L, ArribaNew, Arriba),
    append(Base, L, BaseNew).

% Moverse sacando los vagones de abajo
moverse(vagones, estado(Base, Arriba, Abajo), pop(below, VagonesAMover), estado(BaseNew, Arriba, AbajoNew)) :-
    length(L, VagonesAMover),
    append(L, AbajoNew, Abajo),
    append(Base, L, BaseNew).

% Por la forma en la que se consiguen los movimientos, todos son legales.
legal(vagones, _).