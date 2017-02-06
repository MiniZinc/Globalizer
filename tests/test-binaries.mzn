int:m=5;
int:n=10;

array[1..m,1..n] of var 0..1 : b;

%array[1..m] of var 1..n : x;

constraint forall(i in 1..m) (sum([b[i,j]|j in 1..n])=1);

constraint forall(j in 1..n) (sum([b[i,j]|i in 1..m])<=1);

%constraint forall(i in 1..m, j in 1..n) (x[i]=j <-> b[i,j]=1);

solve satisfy;
