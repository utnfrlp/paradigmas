%
% cinco(X) :- X es el numero 5
%
cinco1(5).
cinco2(5) :- !.
cinco3(X) :- X is 5.
cinco4(X) :- X is 5, true.
cinco5(X) :- X is 5, false.
cinco6(X) :- X is 5, X is 6.
cinco7(X) :- X is 5, 1 == 1.

?- cinco1(X), write('cinco1: '), writeln(X).
?- cinco2(X), write('cinco2: '), writeln(X).
?- cinco3(X), write('cinco3: '), writeln(X).
?- cinco4(X), write('cinco4: '), writeln(X).
?- cinco5(X), write('cinco5: '), writeln(X).
?- cinco6(X), write('cinco6: '), writeln(X).
?- cinco7(X), write('cinco7: '), writeln(X).


%
% ultimo(X,L) :- X es el ultimo elemento de la lista L
%
ultimo(X,[X]).
ultimo(X,[Y|Ys]) :- ultimo(X,Ys).

?- ultimo(X, [1,5,z,7]), write('ultimo: '), writeln(X).


%
% anteultimo(X,L) :- X es el anteultimo elemento de la lista L
%
anteultimo(X, [X,Xs]).
anteultimo(X, [Z, Y|Ys]) :- anteultimo(X, [Y|Ys]).

?- anteultimo(X, [1,5,z,7]), write('anteultimo: '), writeln(X).


%
% kesimo(X,L,K) :- X es el kesimo elemento de la lista L
%
kesimo(X, [X|Xs], 1).
kesimo(X, [Y|Ys], K) :- K > 1, K1 is K - 1, kesimo(X, Ys, K1).

?- kesimo(X, [2,4,6,8], 3), write('kesimo: '), writeln(X).


%
% longitud(L,N) :- la lista L contiene N elementos
%
longitud([],0).
longitud([X|Xs],N) :- longitud(Xs,N1), N is N1 + 1.

?- longitud([2,4,6,8], X), write('longitud: '), writeln(X).


%
% reversa(L1,L2) :- L2 es la lista reversa de L1 
%
reversa(L1,L2) :- reversa(L1,L2,[]).
reversa([],L2,L2).
reversa([X|Xs],L2,Laux) :- reversa(Xs,L2,[X|Laux]).

?- reversa(X, [1,2,3,4,5,6]), write('reversa: '), writeln(X).


%
% palindromo(L) :- L es una lista palindromo
%
palindromo(L) :- reversa(L,L).

?- palindromo("appa"), writeln('appa es palindromo').
?- palindromo("papa"), writeln('papa es palindromo').

%
% remover(X,L,K,R) :- X es el kesimo elemento de la lista L
%    R es la lista que queda cuando el kesimo elemento es removido de L.
%
remover(X,[X|Xs],1,Xs).
remover(X,[Y|Xs],K,[Y|Ys]) :- K > 1, K1 is K - 1, remover(X,Xs,K1,Ys).

?- remover(X, [a,b,c,d], 3, L2), write('remover: '), writeln(L2).


%
% rango(I,K,L) :- I <= K,
% L es la lista que contiene todos los numeros enteros consecutivos entre I y K.
%
rango(I,I,[I]).
rango(I,K,[I|L]) :- I < K, I1 is I + 1, rango(I1,K,L).

?- rango(4,10,L), write('rango: '), writeln(L).
