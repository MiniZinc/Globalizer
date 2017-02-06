include "globals.mzn";

int : n = 5;
array [1..n] of var 1..n : x;
var 1..n : m;

constraint maximum(m, x);

solve satisfy;

output [show(x), show(m)];
