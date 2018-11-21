
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

poly2list(M+P,[P|R]):-poly2list(M,R),!.
poly2list(M-P,[P|R]):-poly2list(M,R),!.
poly2list(M,[M]).
