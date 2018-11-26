:- use_module(library(lists)).
% :-op(500, xfy, '+').
% :-op(500, xfy, '-').

% simplificação de expoentes
% scalepoly

pvars([x,y,z]).
pvar(X):-pvars(V), member(X,V).
pvar(-X):-pvars(V), member(X,V).


power(X^Y):-pvar(X),integer(Y),Y>1,!.
power(X):-pvar(X),!.

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

poly2listA(P-M,[T|R]):- number(M), T is -M, poly2listA(P,R),!.
poly2listA(P-M,[M|-R]):-poly2listA(P,R),!.
poly2listA(P+M,[M|R]):-poly2listA(P,R),!.
poly2listA(M,[M]):- monomial(M),!.
%poly2listA(P*M,[P*M]).
%poly2listA(M,[M]).

%--2--
simpoly(P,P):-
 aux_simpoly(P,P2),
 P==P2,!.
simpoly(P,P3):-
 aux_simpoly(P,P2),
 simpoly(P2,P3),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% aux_simpoly(_*0,0):-!.
% aux_simpoly(0*_,0):-!.
% aux_simpoly(M1*M2,M3):-
%    mulmonomial(M1,M2,M3).
%aux_simpoly(P*M,P2*M2):-aux_simpoly(P,P2),aux_simpoly(M,M2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

takeminus(M1,M2):- number(M1), M1 < 0, M2 is M1*(-1),!.
takeminus(-M,M):-!.
takeminus(-1*M,M):-!.
takeminus(M1,M1).

aux_simpoly(P-0,P):-!.
aux_simpoly(0-M,-M):-monomial(M),!.
aux_simpoly(P-M,M3):-
 monparts(M,_,XExp),
 delmonomial(P,XExp,M2,P2),
 P2 == 0,
 submonomial(M2,M,M3).
aux_simpoly(P-M,P2-R):-
 monparts(M,_,XExp),
 delmonomial(P,XExp,M2,P2),
 submonomial(M2,M,M3), takeminus(M3,R).


aux_simpoly(P-M,P2-M3):-
monparts(M,_,XExp),
delmonomial(P,XExp,M2,P2),!,
submonomial(M2,M,M3).
aux_simpoly(P-M,P2-M2):-aux_simpoly(P,P2),simmon(M,M2).


aux_simpoly(P+0,P):-!.
aux_simpoly(0+M,M):-monomial(M),!.

aux_simpoly(P+M,P2-R):-
  number(M),
   monparts(M,_,XExp),
   delmonomial(P,XExp,M2,P2),
   addmonomial(M,M2,M3),
   M3 < 0,
   takeminus(M3,R).

aux_simpoly(P+M,P2+M3):-
   monparts(M,_,XExp),
   delmonomial(P,XExp,M2,P2),!,
   addmonomial(M,M2,M3).
aux_simpoly(P+M,P2+M2):-aux_simpoly(P,P2),simmon(M,M2).


aux_simpoly(M,M2):-monomial(M), simmon(M,M2),!.

simmon(1*P,P):- power(P),!.
simmon(-1*P,-P):-power(P),!.
simmon(P*1,P):- power(P),!.
simmon(0-P,-P):-!.
simmon(0*_,0):-!.
simmon(_*0,0):-!.
%simmon(K1*K2,R):-number(K1), number(K2), R is K1*K2,!.
simmon(M,M).

mult(K1*K2,R):-number(K1), number(K2), R is K1*K2,!.

monparts(X^N,1,X^N):-power(X^N),!.
monparts(K*P,K,P):-number(K),!.
monparts(P*K,K,P):-number(K),!.
monparts(K,K,indep):-number(K),!.
monparts(X,1,X):-pvar(X),!.

expmonparts(X^N,N,X):-power(X^N),!.

delmonomial(M,X,M,0):-
 monomial(M),monparts(M,_,X),!.
delmonomial(M-M2,X,M,NM):-
 number(M2),monomial(M),monparts(M,_,X),
 NM is -M2,!.
delmonomial(P-M,X,NM,P):-
 number(M),NM is -M, monparts(NM,_,X),!.


delmonomial(M-K*M2,X,M,NK*M2):-
monomial(M2),monomial(M),monparts(M,_,X), NK is -K,!.
delmonomial(P-M,X,-M,P):-
monomial(M),monparts(M,_,X),!.


delmonomial(M-M2,X,M,-M2):-
monomial(M2),monomial(M),monparts(M,_,X),!.
delmonomial(P-M,X,-M,P):-
monomial(M),monparts(M,_,X),!.

delmonomial(P-M2,X,M,P2-M2):-
 delmonomial(P,X,M,P2).

delmonomial(M+M2,X,M,M2):-
 monomial(M2),monomial(M),monparts(M,_,X),!.
delmonomial(P+M,X,M,P):-
 monomial(M),monparts(M,_,X),!.
delmonomial(P+M2,X,M,P2+M2):-
 delmonomial(P,X,M,P2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mulmonomial(K1,K2,NK1*NK2):-
  not(number(K2)),
  takeminus(K2,NK2),
  NK2 \= K2,!,
  NK1 is K1*(-1).

mulmonomial(K1,K2,NK2*NK1):-
  not(number(K1)),
  takeminus(K1,NK1),
  NK1 \= K1,!,
  NK2 is K2*(-1).

mulmonomial(K1,K2,K3):-
 number(K1),number(K2),!,
 K3 is K1*K2.
mulmonomial(K1,M2,M3):-
 number(K1),
 monparts(M2,K2,XExp),
 K3 is K1*K2,
 aux_mulmonomial(K3,XExp,M3),!.
mulmonomial(M1,K2,M3):-
 number(K2),
 monparts(M1,K1,XExp),
 K3 is K1*K2,
 aux_mulmonomial(K3,XExp,M3),!.
mulmonomial(M1,M2,M3):-
 monparts(M1,K1,XExp),
 monparts(M2,K2,XExp),
 K3 is K1+K2,
 exp_mulmonomial(K3,XExp,M3).

exp_mulmonomial(K,indep,K):-!.
exp_mulmonomial(0,_,1):-!.
exp_mulmonomial(1,XExp,XExp):-!.
exp_mulmonomial(K,XExp,XExp^K).

aux_mulmonomial(K,indep,K):-!.
aux_mulmonomial(0,_,0):-!.
aux_mulmonomial(1,XExp,XExp):-!.
aux_mulmonomial(K,XExp,K*XExp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addmonomial(-K1,K2,K3):-
  K3 is K2-K1.
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

submonomial(-K1,K2,K3):-
  number(K1),number(K2),!,
  K3 is -(K1+K2).
submonomial(K1,K2,K3):-
 number(K1),number(K2),!,
 K3 is K1-K2.
submonomial(M1,M2,M3):-
 monparts(M1,K1,XExp),
 monparts(M2,K2,XExp),
 K3 is K1-K2,
 aux_submonomial(K3,XExp,M3).


aux_submonomial(K,indep,K):-!.
aux_submonomial(0,_,0):-!.
aux_submonomial(1,XExp,XExp):-!.
aux_submonomial(K,XExp,K*XExp).

%--3--
simpoly_list(L1,L2):-poly2list(P, L1), simpoly(P,P2), poly2list(P2,L2).

%--4--
scalepoly(P1,K,P3):-number(K), poly2list(P1,LP1), aux_scalepoly(LP1,K,LP3), poly2list(TSP3,LP3), simpoly(TSP3,P3).

aux_scalepoly([],_,[]).
aux_scalepoly([M|P],K,[RM|P2]):- mulmonomial(M,K,RM), aux_scalepoly(P,K,P2).




% aux_scalepoly(M,K,T*M2):- monparts(M,M1,M2), T is M1*K.
%
%
% scalepoly(P+M,K,P2+M2):-number(K), aux_scalepoly(M,K,P2), scalepoly(P,K,M2).



% poly2listA(P-M,[T|R]):- number(M), T is -M, poly2listA(P,R),!.
% poly2listA(P-M,[M|-R]):-poly2listA(P,R),!.
% poly2listA(P+M,[M|R]):-poly2listA(P,R),!.
% poly2listA(M,[M]):- monomial(M),!.

%--5--
% addpoly(P1,P2,R):- simpoly(P1+P2,R).
addpoly(X,Y,Z):- poly2list(X,XL), poly2list(Y,YL), append(XL,YL,RL), poly2list(RP,RL), simpoly(RP,Z).
