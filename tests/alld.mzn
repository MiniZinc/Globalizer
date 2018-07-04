int: n = 6;
int: m = n;
array[1..n] of var 1..n : x;

constraint forall(i in 1..n, j in 1..m where i<j) (x[i] != x[j] /\ i < j);

output [show(x)];

solve satisfy;
