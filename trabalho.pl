:- use_module(library(lists)).


pvars([x,y,z]).
pvar(X):-pvars(V), member(X,V).

power(X):-pvar(X),!.
power(X^Y):-pvar(X),integer(Y),Y>1,!.

coefficient(K):-number(K).


monomial(X):-pvar(X),!.
monomial(N):-number(N),!.
monomial(X):-power(X),!.
monomial(K*X):-coefficient(K),power(X),!.

polynomial(M):-monomial(M),!.
polynomial(P+M):-monomial(M),polynomial(P),!.


% poly2list(M+P,[M|R]):-monomial(M), poly2list(P,R),!.
% poly2list(M-P,[M|R]):-monomial(M),poly2list(P,R),!.
% poly2list(M,[M]):-monomial(M),!.

% poly2list(P+M,[M|R]):-poly2list(P,R),!.
% poly2list(P-M,[M|R]):-poly2list(P,R),!.
% poly2list(M,[M]):-monomial(M).

poly2list([],L).
poly2list(P+M,L):-poly2list(P,[M|L]),!.
poly2list(P-M,L):-poly2list(P,[M|L]),!.
poly2list(M,L):-poly2list([],[M]),!.


% poly2list(P+M,L):-append(L,M,X),poly2list(P,X),!.
% poly2list(P-M,L):-append(L,M,X),poly2list(P,X),!.
% poly2list(M,[M]):-monomial(M),!.


simpoly_list().

simpoly(M,M2):-monomial(M), simmono(M,M2),!.
simpoly(P+0,P):-!.
simpoly(0+P,P):-monomial(P),!.
simpoly(P+M,P2+M2):-simpoly(P,P2), simmono(M,M2).

simmono(1*P,P):- power(P),!.
simmono(0*_,0):-!.
simmono(M,M).

scalepoly().

addpoly().
