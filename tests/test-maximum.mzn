int : n = 5;
array [1..n] of var 1..n : x;
var 1..n : m;

constraint forall (i in 1..n) (x[i] <= m);
constraint exists (j in 1..n) (x[j] = m);

solve satisfy;

output [show(x), show(m)];
