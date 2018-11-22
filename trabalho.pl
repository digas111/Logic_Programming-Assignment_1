:- use_module(library(lists)).
:-op(500, xfy, '+').

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


% poly2list(M+P,[M|R]):-monomial(M), poly2list(P,R),!.
% poly2list(M-P,[M|R]):-monomial(M),poly2list(P,R),!.
% poly2list(M,[M]):-monomial(M),!.

poly2list(M,[M]):-monomial(M).
poly2list(M+P,[M|R]):-poly2list(P,R),!.
poly2list(M-P,[M|R]):-poly2list(P,R),!.

% poly2list([],L).
% poly2list(P+M,[M|L]):-poly2list(P,[M|L]),!.
% poly2list(P-M,[M|L]):-poly2list(P,[M|L]),!.
% poly2list(M,[M|L]):-poly2list([],[M|L]),!.


% poly2list(P+M,L):-append(L,M,X),poly2list(P,X),!.
% poly2list(P-M,L):-append(L,M,X),poly2list(P,X),!.
% poly2list(M,[M]):-monomial(M),!.


simpoly_list().

% simpoly(M,M2):-monomial(M), simmon(M,M2),!.
simpoly(P+0,P):-!.
simpoly(0+P,P):-monomial(P),!.
simpoly(P+M,P2+M2):-simpoly(P,P2), simmon(M,M2).
simpoly(P+M,P2+M3):-
    monparts(M,_,XExp),
    delmonomial(P,XExp,M2,P2),!,
    addmonomial(M,M2,M3).
simpoly(P+M,P2+M2):-simpoly(P,P2),simmon(M,M2).

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
    monparts(M1,K1,XExp),
    K3 is K1+K2,
    aux_addmonomial(K3,XExp,M3).

aux_addmonomial(K,indep,K):-!.
aux_addmonomial(0,_,0):-!.
aux_addmonomial(1,XExp,XExp):-!.
aux_addmonomial(K,XExp,K*XExp).

simmon(1*P,P):- power(P),!.
simmon(0*_,0):-!.
simmon(M,M).

scalepoly().

addpoly().
