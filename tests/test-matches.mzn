int : n = 5;
constraint forall (i,j in 1..n) (i+j < 25);
constraint forall (k,l in 1..n) (k-l > -20);
solve satisfy;
