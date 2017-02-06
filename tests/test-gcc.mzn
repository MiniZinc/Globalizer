include "globals.mzn";

int : n = 5;
array[1..2*n] of var 1..n : x;
array[1..n] of var 0..2*n : c;

constraint forall (i in 1..n) (count(x, i, c[i]));

solve satisfy;
