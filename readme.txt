Casos que dão:

poly2list(2+3,L).
poly2list(2*x+3,L).
poly2list(2*x+3*y+3,L).
poly2list(2+2*x-3,L).
poly2list(-2*x-3,L).
poly2list(2+2*x(-3*x)+y+3,L).
poly2list(-2*x+(-3*y),L).

simpoly(2+2*x-3*x+y+3,P).

Casos que não dão:
simpoly((x^2)+4*x^2,P).
simpoly(x+x,P).