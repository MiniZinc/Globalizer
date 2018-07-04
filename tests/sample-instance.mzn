int : n = 5;
set of int : S = 1..n;

array [S] of var S : x;
array [S] of var S : y;

constraint forall (i,j in S where i<j)
  (x[i] + i != x[j] + j /\ y[i] + i != y[j] + j);

solve :: int_search(x, input_order, indomain_random, complete) satisfy;

output [show([x[1], x[2], x[3], x[4], n, x[5], y[1], y[2], y[3], y[4], y[5]])];
