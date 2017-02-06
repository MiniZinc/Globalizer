int : n;

array [1..n] of var 1..n : x;
array [1..n] of var 1..n : y;

constraint forall (i,j in 1..n) (x[i] = j <-> y[j] = i);

solve satisfy;

output [show(x), show(y)];
