include "globals.mzn";

var 1..10 : x;
array [1..10] of var 0..1 : b;

constraint forall (i in index_set(b)) (b[i] = 1 <-> x = i);

solve satisfy;
