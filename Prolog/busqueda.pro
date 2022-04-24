% Autores:
% Jesus Bandez 17-10046
% Mariangela Rizzo 17-10538

%%%%%%%%%%%% Motor de busqueda IDDFS %%%%%%%%%%%%%%%
iddfs(Problema, Inicial, Movimientos) :-
    % Se inicializa creando una clausula que indica que hay mas nodos por explorar
    asserta(hayMasNodos(true)),
    % Llamada inicial con profundidad 0
    iddfs(Problema, Inicial, [Inicial], Movimientos, 0).

% Si no hay mas nodos por explorar, se acaba la busqueda.
iddfs(_, _, _, _, _) :-
    \+ hayMasNodos(true),
    fail.

% Si hay mas nodos por explorar en la profundidad actual, se exploran
iddfs(Problema, Estado, Historia, Movimientos, Depth) :-
    retract(hayMasNodos(true)),
    ldfs(Problema, Estado, Historia, Movimientos, Depth).

% Si no se pudo conseguir la respuesta con la profundidad actual, se busca
% en el siguiente nivel de profundidad
iddfs(Problema, Estado, Historia, Movimientos, Depth) :- 
    retract(hayMasNodos(true)),
    Depth0 is Depth+1,
    iddfs(Problema, Estado, Historia, Movimientos, Depth0).

% Si se llega a un estado que es el final, se ha encontrado la solucion
ldfs(Problema, Estado, _, [], _) :-
    final(Problema, Estado).

% Si se llega a la profundidad 0 y no se ha encontrado la solucion, se comprueba
% que se pueda obtener un estado legal que no exista en la historia. Esto es,
% se comprueba si aun queda otro estado por explorar. Si existe, se crea una
% clausula que confirma que quedan estados por explorar. En caso contrario,
% la clausula no es creada.
ldfs(Problema, Estado, Historia, [], 0) :-
    \+ hayMasNodos(true),
    movimiento(Problema, Estado, Movimiento),
    moverse(Problema, Estado, Movimiento, Proximo),
    legal(Problema, Proximo),
    \+ member(Proximo, Historia),
    asserta(hayMasNodos(true)),
    fail.

% Se exploran los nodos desde la profundidad Depth hasta la profundidad 0
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


:- discontiguous([inicial/2, final/2, movimiento/3, moverse/4, legal/2]).
:- dynamic inicial/2.
:- dynamic final/2.
:- dynamic hayMasNodos/1.

%%%%%%%%%%%%%%    Problemas de los canales  %%%%%%%%%%%%%
% Los estados son el functor estado(Televisores, N, actual, last), donde Televisores son los canales
% actuales de cada televisor, N la cantidad de canales posibles, actual es el televisor al que se le
% cambio recietemente el canal y last es el televisor al que se le cambio el canal antes
% del ultimo televisor cambiado de canal
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
    direccion(Direccion),
    between(1, Len, VagonesAMover).
    

direccion(below).
direccion(above).


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

% Por la forma en la que se consiguen los movimientos, todos los estados conseguidos son legales
legal(vagones, _).