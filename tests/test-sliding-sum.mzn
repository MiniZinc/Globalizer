array [1..5,1..10] of var 0..100 : x;
int : width = 3;
int : bound = 2;

constraint forall (r in 1..5)
  (forall (i in 1..8)
    (sum(j in 1..width)(x[r,i+j-1]) <= bound));

solve satisfy;

output [show(x)];
