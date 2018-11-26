Para testar é necessário ter o swipl instalado na máquina, para carregar o ficheiro "[trabalho]."

Casos que dão:


poly2list(2+3,L).
poly2list(2*x+3,L).
poly2list(2*x+3*y+3,L).
poly2list(2+2*x-3,L).
poly2list(-2*x-3,L).
poly2list(2+2*x(-3*x)+y+3,L).
poly2list(-2*x+(-3*y),L).

simpoly(2+2*x-3*x+y+3,P).
simpoly(x+x,P).
simpoly((x^2)+4*x^2,P).
simpoly(x^2-4*x^2,P).
simpoly(-4-3,P).


simpoly_list([-4,-3],L).
simpoly_list([-2*x,-3],L).
simpoly_list([2,2*x,-3*x,y,3],L).
simpoly_list([-3*x,3*x, x^2],L).

scalepoly(x,3,P).
scalepoly(x+y+z,3,P).
scalepoly(x+4+2*x,3,P).
scalepoly(-2*x+2,3,P).
scalepoly(-2*x^2+x^2+2,3,P).

addpoly(x+3, y+1,P).
addpoly(x^2+3, x^2+1,P).
addpoly(x^2+y+3, x^2+1,P).


Casos que não dão:
?- simpoly(-x^2-4*x^2,P).
false.


?- scalepoly(x+4-2*x,3,P).
Fica em loop

?- addpoly(x^2-y+3, x^2-1,P).
Fica em loop

?- addpoly(-x^2+y+3, x^2-1,P).
Fica em loop

Casos com solução quase perfeita:

?- simpoly_list([-3*x,3*x, -x^2],L).
L = [0, -x^2].

?- scalepoly(-2*x-2,3,P).
P = -6*x+ -6.
