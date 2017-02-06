int : n = 5;
array [1..n] of var 1..n : x;
int : m = 3;

constraint forall (i in 1..n) (x[i] <= 3);
constraint exists (j in 1..n) (x[j] = 3);

solve satisfy;

