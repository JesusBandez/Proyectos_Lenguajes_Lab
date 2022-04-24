% Autores:
% Jesus Bandez 17-10046
% Mariangela Rizzo 17-10538

%%%% Motor de busqueda IDDFS

% Predicado auxiliar que crea un atomo en la base de datos.
% Si el atomo existe, lo crea y lo vuelve a eliminar.
existenNodos(X) :-
    retract(X), !, asserta(X).
existenNodos(X) :-
    asserta(X).

% busqueda iddfs. Crea el atomo indicando que hay nodos por explorar
% y luego llama al predicado auxiliar para iniciar la busqueda con 
% profundidad 0
iddfs(Problema, Inicial, Movimientos) :-
    existenNodos(hayNodos),
    iddfs(Problema, Inicial, [Inicial], Movimientos, 0).

% Si el atomo "hayNodos" existe en la base de datos, es porque hay nodos por explorar.
% Entonces, se borra el atomo de la base de datos y se exploran los nodos.
% Si el atomo no existe en la base de datos, entonces no hay mas nodos por explorar y
% la busqueda falla
iddfs(_, _, _, _, _) :-
    \+ retract(hayNodos), !, fail.

% Se llama a la busqueda desde la profundidad "Depth".
iddfs(Problema, Estado, Historia, Movimientos, Depth) :-
    ldfs(Problema, Estado, Historia, Movimientos, Depth).

% Si la busqueda en la profundidad Depth falla, entonces se busca
% en la profundidad Depth + 1.
iddfs(Problema, Estado, Historia, Movimientos, Depth) :-
    Depth0 is Depth+1,
    iddfs(Problema, Estado, Historia, Movimientos, Depth0).

% Al llegar a la profundidad 0, pueden haber mas nodos. Se crea el atomo
% que indica que hay mas nodos y se comprueba si está en el estado final.
ldfs(Problema, Estado, _, [], 0) :-
    existenNodos(hayNodos),
    final(Problema, Estado).

% Se ejecuta la busqueda en el nivel Depth y se procede a buscar en Depth-1
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
:- dynamic hayNodos/0.

%%%%%%%%%%%%%%    Problemas de los canales  %%%%%%%%%%%%%
% Los estados son el functor estado(Televisores, N, actual, last), donde Televisores son los canales
% actuales de cada televisor, N la cantidad de canales posibles, actual es el televisor al que se le
% cambio recietemente el canal y last es el televisor al que se le cambio el canal antes
% del ultimo televisor cambiado de canal
canales(Televisores, N, L) :-
    asserta(inicial(controlRemoto, estado(Televisores, N, -1, -1))),
    resolver(controlRemoto, L),
    retractall(inicial(controlRemoto, _)).

% Predicado que se encarga de limpiar las clausulas creadas en caso de que
% resolver falle.
canales(_, _, _) :-
    retractall(inicial(controlRemoto, _)), !,
    fail.

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

% Predicado que se encarga de limpiar las clausulas creadas en caso de que
% resolver falle.
vagones(_, _, _) :-
    retractall(final(vagones, _)),
    retractall(inicial(vagones, _)), !,
    fail.


% Todos los movimientos posibles de push
movimiento(vagones, estado([Vagon|R], _, _), push(Direccion, VagonesAMover)) :-
    length([Vagon|R], Len),
    direccion(Direccion),
    between(1, Len, VagonesAMover).
    
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

% Por la forma en la que se consiguen los movimientos, todos los estados conseguidos son legales
legal(vagones, _).