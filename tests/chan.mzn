int : m = 4;
int : n = 3;

array[1..n, 1..m] of var 0..1 : x;

constraint forall (i in 1..n) (sum([(x[i,j]) | j in 1..n]) = 1);

constraint forall (j in 1..m) (sum([(x[i,j]) | i in 1..n]) <= 1);

solve satisfy;

output [show(x)];
