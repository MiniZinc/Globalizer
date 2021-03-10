int : n = 10;
array [1..n] of var 1..n : x;
array [1..n] of var 1..n : y;

int: q = 1;

constraint if q == 1 then
             forall(k in 1..2) (
               if k == 1 then
                 forall (i,j in 1..n where i<j) (x[i] != x[j])
               else
                 forall (i,j in 1..n where i<j) (y[i] != y[j])
               endif)
           else true endif;

% constraint forall (i,j in 1..n where i<j) (x[i] != x[j]);
% constraint forall (i,j in 1..n where i<j) (y[i] != y[j]);

solve satisfy;

output [show(x), show(y)];
