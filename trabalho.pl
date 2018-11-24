:- use_module(library(lists)).
% :-op(500, xfy, '+').
% :-op(500, xfy, '-').

pvars([x,y,z]).
pvar(X):-pvars(V), member(X,V).

power(X):-pvar(X),!.
power(X^Y):-pvar(X),integer(Y),Y>1,!.

coefficient(K):-number(K).

monomial(X):-pvar(X),!.
monomial(N):-number(N),!.
monomial(X):-power(X),!.
monomial(K*X):-coefficient(K),power(X),!.
monomial(X*K):-coefficient(K),power(X),!.

polynomial(M):-monomial(M),!.
polynomial(P+M):-monomial(M),polynomial(P),!.

%--1--
poly2list(P,L):-reverse(X,L),poly2listA(P,X),!.

poly2listA(P-M,[-M|R]):-poly2listA(P,R),!.
poly2listA(P+M,[M|R]):-poly2listA(P,R),!.
poly2listA(M,[M]):- monomial(M).

%--2--
simpoly(P,P):-
  aux_simpoly(P,P2),
  P==P2,!.
simpoly(P,P3):-
  aux_simpoly(P,P2),
  aux_simpoly(P2,P3),!.

aux_simpoly(M,M2):-monomial(M), simmon(M,M2),!.
aux_simpoly(P+0,P):-!.
aux_simpoly(0+P,P):-monomial(P),!.
aux_simpoly(P+M,P2+M3):-
    monparts(M,_,XExp),
    delmonomial(P,XExp,M2,P2),!,
    addmonomial(M,M2,M3).
aux_simpoly(P+M,P2+M2):-aux_simpoly(P,P2),simmon(M,M2).

simmon(1*P,P):- power(P),!.
simmon(0*_,0):-!.
simmon(M,M).

monparts(X^N,0,X^N):-power(X^N),!.
monparts(K*P,K,P):-number(K),!.
monparts(K,K,indep):-number(K),!.
monparts(X,1,X):-pvar(X),!.

delmonomial(M,X,M,0):-
  monomial(M),monparts(M,_,X),!.
delmonomial(M+M2,X,M,M2):-
  monomial(M2),monomial(M),monparts(M,_,X),!.
delmonomial(P+M,X,M,P):-
  monomial(M),monparts(M,_,X),!.
delmonomial(P+M2,X,M,P2+M2):-
  delmonomial(P,X,M,P2).

addmonomial(K1,K2,K3):-
  number(K1),number(K2),!,
  K3 is K1+K2.
addmonomial(M1,M2,M3):-
    monparts(M1,K1,XExp),
    monparts(M2,K2,XExp),
    K3 is K1+K2,
    aux_addmonomial(K3,XExp,M3).

aux_addmonomial(K,indep,K):-!.
aux_addmonomial(0,_,0):-!.
aux_addmonomial(1,XExp,XExp):-!.
aux_addmonomial(K,XExp,K*XExp).

%--3--
simpoly_list(L1,L2):-poly2list(P, L1), simpoly(P,P2), poly2list(P2,L2).

%--4--
scalepoly().

%--5--
addpoly().
