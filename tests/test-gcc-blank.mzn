include "globals.mzn";

int : n = 4;
int : m = ((n+1)*n) div 2;
array[1..m] of var 1..n : x;

constraint forall (i in 1..n) (count(x, i, i));

% constraint increasing(x);

solve satisfy;

output [show(x)];
