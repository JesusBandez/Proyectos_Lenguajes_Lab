% Definiendo operadores
:-  op(1, xfx, \\).
:-  op(700, xfx, isr).
:-  op(200, fy, ~).

% Asigna a X el valor de evaluar y simplificar la expresión E (que representa operaciones con racionales).
X isr E :-
    once(eval(E,R)),
    once(simplify(R,X)).

% Funcion auxiliar que obtiene el minimo comun divisor de dos numeros positivos.
mcd(X,X,X).
mcd(A,B,M) :- 
    A > B, 
    A1 is A - B, 
    mcd(A1,B,M).
mcd(A,B,M) :- 
    B > A, 
    B1 is B - A, 
    mcd(A,B1,M).

% Evalua una expresión que representa operaciones con racionales y retorna el resultado no simplificado.
eval(not_a_number, not_a_number).
eval(infinity, infinity).
eval(0\\0, not_a_number).
eval(N\\0, infinity) :-
    N \= 0.
eval(N, N\\1) :-
    integer(N).
eval(~N, 1\\N) :-
    integer(N).
eval(N\\D, N\\D) :-
    D \= 0.
eval(~N\\D, R) :-
    eval(D\\N, R).
eval(X + Y, R) :-
    eval(X,R1), eval(Y,R2),
    sumr(R1, R2, R).
eval(X - Y, R) :-
    eval(X,R1), eval(Y,R2),
    restr(R1,R2,R).
eval(X * Y, R) :-
    eval(X,R1), eval(Y,R2),
    mulr(R1,R2,R).
eval(X / Y, R) :-
    eval(X,R1), eval(Y,R2),
    divr(R1,R2,R).

% Realiza la suma de los racionales o propaga infinity / not_a_number
sumr(infinity, _, infinity).
sumr(_, infinity, infinity).
sumr(not_a_number, _, not_a_number).
sumr(_, not_a_number, not_a_number).
sumr(N1\\D1, N2\\D2, R\\DR) :- 
    DR is D1 * D2,
    R is N1 * D2 + N2 * D1.

% Realiza la resta de los racionales o propaga infinity / not_a_number
restr(infinity, _, infinity).
restr(_, infinity, infinity).
restr(not_a_number, _, not_a_number).
restr(_, not_a_number, not_a_number).
restr(N1\\D1, N2\\D2, R\\DR) :- 
    DR is D1 * D2,
    R is N1 * D2 - N2 * D1.

% Realiza la multiplicacion de los racionales o propaga infinity / not_a_number
mulr(infinity, _, infinity).
mulr(_, infinity, infinity).
mulr(not_a_number, _, not_a_number).
mulr(_, not_a_number, not_a_number).
mulr(N1\\D1, N2\\D2, R\\DR) :- 
    DR is D1 * D2,
    R is N1 * N2.

% Realiza la division de los racionales o propaga infinity / not_a_number
divr(infinity, _, infinity).
divr(_, infinity, infinity).
divr(not_a_number, _, not_a_number).
divr(_, not_a_number, not_a_number).
divr(N1\\D1, N2\\D2, R\\DR) :- 
    DR is N2 * D1,
    R is N1 * D2.

% Simplifica la expresión evaluada. Algunos ejemplos:
% simplify((-2)\\(-4),1\\2).
% simplify(30\\100, 3\\10).
% simplify(-30\\100, -3\\10).
% simplify(30\\(-100), -3\\10).
% simplify(2\\1, 2).
% simplify(0\\1, 0).
simplify(infinity,infinity).
simplify(not_a_number,not_a_number).
simplify(0\\_,0).
simplify(N\\1,N).
simplify(N\\D,S) :-
    N > 0, D < 0,
    Da is abs(D),
    Na is N * (-1),
    simplify(Na\\Da,S).
simplify(N\\D,S) :-
    N < 0,  D < 0,
    Na is abs(N), Da is abs(D),
    simplify(Na\\Da,S).
simplify(N\\D,S) :-
    Na is abs(N), Da is abs(D),
    Da \= 1,
    once(mcd(Na,Da,1)),
    S = N \\ D.
simplify(N\\D,S) :-
    Na is abs(N), Da is abs(D),
    once(mcd(Na,Da,M)),
    M \= 1,
    NR is N / M, DR is D / M,
    simplify(NR\\DR,S).