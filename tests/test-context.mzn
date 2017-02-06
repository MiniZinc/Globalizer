annotation booleanises(array[int,int] of var int : x);

int : n=3;
int : m=4;

array[1..n, 1..n, 1..m] of var 0..1 : b :: booleanises(x);
array[1..n, 1..n] of var 1..m : x;

% channeling
constraint forall (aa,bb in 1..n, cc in 1..m) (x[aa,bb]=cc <-> b[aa,bb,cc]=1);

constraint forall (i1 in 1..n)
  ( forall (v in 1..m)
    ( sum([b[i1,i2,v] | i2 in 1..n]) <= 1 ));

% desired result:
% forall (i1 in 1..n) (alldifferent(x[i1,_]))

% Note the potential problem here: i1 is never used in the model as an
% argument to x.  This can be a problem if we filter array indices
% aggresively.

output [show([x[1,j] | j in 1..n]), "\n", show([b[1,j,k]|j in 1..n, k in 1..m])];

solve satisfy;
