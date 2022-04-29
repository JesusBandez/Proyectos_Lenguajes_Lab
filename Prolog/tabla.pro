:- op( 500, fy, :~:).     % negacion
:- op(1000, xfy, :/\:).    % and
:- op(1100, xfy, :\/:).  % or
:- op(1110, xfy, :=>:).   % implicacion
:- op(1120, xfy, :<=>:).   % doble implicacion

% loop que pide una formula con un prompt.
truthtable :-
    repeat,
    write('formula '), read(F),
    procesar(F), !.

truthtablepro :-
    repeat,
    write('formula '), read(F),
    procesarpro(F), !.

% loop que procesa el comando insertado.
procesar(bye).
procesar(F) :-
    tabla(F),
    fail.

procesarpro(bye).
procesarpro(F) :-
    tablapro(F),
    fail.

% Hace los calculos necesarios para producir la tabla de la verdad.
tabla(F) :-
    variables(F, V),
    separador(V),
    header_tabla(V),
    separador(V),
    length(V,N), length(B,N),
    bagof(B, bools(B), Bs),
    contenido_tabla(F,V,Bs),
    separador(V).

tablapro(F) :-
    variables(F, V),
    separador(V, F),
    header_tabla(V, F),
    separador(V, F),
    length(V,N), length(B,N),
    bagof(B, bools(B), Bs),
    contenido_tablapro(F,V,Bs),
    separador(V, F).

% Separa las filas de la tabla
separador([]) :- writeln('+---+').
separador([_|VR]) :-
    write('+---'),
    separador(VR).

separador([], F) :- 
    write('+-'),
    term_to_atom(F, A),
    atom_chars(A, C),
    separadorpro(C),
    writeln('-+').
separador([V|VR], F) :-
    write('+-'),
    atom_chars(V, VL),
    separadorpro(VL),
    write('-'),
    separador(VR, F).

separadorpro([]).
separadorpro([_|CR]) :-
    write('-'),
    separadorpro(CR).


% Muestra en consola las variables y la formula en el formato de la tabla.
header_tabla([]) :- writeln('| F |').
header_tabla([V|VR]) :-
    write('| '),
    write(V),
    write(' '),
    header_tabla(VR).

header_tabla([], F) :- 
    write('| '), 
    write(F), 
    writeln(' |').
header_tabla([V|VR], F) :-
    write('| '),
    write(V),
    write(' '),
    header_tabla(VR, F).

% Loop sobre todos los posibles valores de las variables que calcula su resultado y lo muestra en consola en el formato de la tabla.
contenido_tabla(_,_,[]).
contenido_tabla(F,V,[B|BR]) :-
    evalb(F,V,B,R),
    contenido_tabla(B,R),
    contenido_tabla(F,V,BR).

contenido_tablapro(_,_,[]).
contenido_tablapro(F,V,[B|BR]) :-
    evalb(F,V,B,R),
    contenido_tablapro(B,R,V,F),
    contenido_tablapro(F,V,BR).

% Muestra en consola los valores booleanos de cada variable del predicado y el resultado de evaluarlo en formato de tabla.
contenido_tabla([], R) :- 
    write('| '),
    write(R),
    write(' |'), nl.
contenido_tabla([B|BR], R) :-
    write('| '),
    write(B),
    write(' '),
    contenido_tabla(BR, R).

contenido_tablapro([], R, [], F) :- 
    write('|'),
    term_to_atom(F, A),
    atom_chars(A, C),
    length(C,N),
    contenido_tablapro_formater(C, R, N),
    write(' |'), nl.
contenido_tablapro([B|BR], R, [V|VR], F) :-
    write('|'),
    atom_chars(V, VL),
    length(VL,N),
    contenido_tablapro_formater(VL, B, N),
    write(' '),
    contenido_tablapro(BR, R, VR, F).

contenido_tablapro_formater([], _, _).
contenido_tablapro_formater([C|CR], B, N) :-
    Z is N // 2,
    length([C|CR], Z),
    write(B),
    write(' '),
    contenido_tablapro_formater(CR, B, N).
contenido_tablapro_formater([C|CR], B, N) :-
    Z is N // 2,
    length([C|CR], N2),
    Z \= N2,
    write(' '),
    contenido_tablapro_formater(CR, B, N).


%%%%% Funciones usadas para el cálculo de la truthtable %%%%

% Predicado auxiliar, triunfa si el elemento E no está en la lista indicada.
not_member(_,[]) :- !.
not_member(E,[H|R]) :-
    E \= H,
    not_member(E,R).

% Triunfa si V es una lista con todas las variables de la expresión lógica E.
variables(E,V) :- 
    variables(E,[],Vt),
    reverse(Vt, V).

% Si la variable ya fue considerada, no se añade.
variables(X,Vi,V) :-
    atom(X), 
    member(X,Vi),
    V = Vi.
% Si no ha sido considerada, se añade.
variables(X,Vi,V) :-
    atom(X), 
    not_member(X,Vi),
    V = [X|Vi].
% negacion
variables(:~: X,Vi,V) :-
    variables(X,Vi,V).
% and
variables(X :/\: Y,Vi,V) :-
    variables(X,Vi,Vt),
    variables(Y,Vt,V).
% or
variables(X :\/: Y,Vi,V) :-
    variables(X,Vi,Vt),
    variables(Y,Vt,V).
% implicacion
variables(X :=>: Y,Vi,V) :-
    variables(X,Vi,Vt),
    variables(Y,Vt,V).
% doble implicacion
variables(X :<=>: Y,Vi,V) :-
    variables(X,Vi,Vt),
    variables(Y,Vt,V).

% Tabla de la verdad base para nuevos operadandes definidos
% negacion
negacion(t,f).
negacion(f,t).
% and
and(t,t,t).
and(t,f,f).
and(f,t,f).
and(f,f,f).
% or
or(t,t,t).
or(t,f,t).
or(f,t,t).
or(f,f,f).
% implicacion
implicacion(t,t,t).
implicacion(t,f,f).
implicacion(f,t,t).
implicacion(f,f,t).
% doble implicacion
doble_implicacion(t,t,t).
doble_implicacion(t,f,f).
doble_implicacion(f,t,f).
doble_implicacion(f,f,t).

% Evaluar una expresión booleana

% Predicado auxiliar que triunfa al encontrar el valand V de la variable X.
buscar(X,[X|_],[V|_],V).
buscar(X,[_|XR],[_|VR],V) :- buscar(X,XR,VR,V).


evalb(X,_,_,X) :- 
    member(X,[t,f]).
evalb(X,V,A,R) :- 
    atom(X),
    buscar(X,V,A,R).
% negacion
evalb(:~: X,V,A,R) :-   
    evalb(X,V,A,R1),
    negacion(R1,R).
% and
evalb(X :/\: Y,V,A,R) :- 
    evalb(X,V,A,R1),
    evalb(Y,V,A,R2),
    and(R1,R2,R).
% or
evalb(X :\/: Y,V,A,R) :-  
    evalb(X,V,A,R1),
    evalb(Y,V,A,R2),
    or(R1,R2,R).
% implicacion
evalb(X :=>: Y,V,A,R) :- 
    evalb(X,V,A,R1),
    evalb(Y,V,A,R2),
    implicacion(R1,R2,R).
% doble implicacion
evalb(X :<=>: Y,V,A,R) :- 
    evalb(X,V,A,R1),
    evalb(Y,V,A,R2),
    doble_implicacion(R1,R2,R).

% Valores booleanos de cada una de las variables en la tabla.
bool(t).
bool(f).

% Recibe una lista de variables no ligadas y genera todas las posibles combinaciones de bool.
bools([]).
bools([B | BR]) :-
    bool(B),
    bools(BR).
    