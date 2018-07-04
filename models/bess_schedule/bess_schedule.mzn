int: d;
int: n;
array[1..n] of int: m;
int: maxR;
int: horizon = 2*(n-1);
int: makespan = horizon+d;


array[1..n] of var 0..2*(n-1): x;
array[0..2*(n-1),1..n] of var 0..maxR: R;

constraint forall (i,j in 1..n where i<j) ((x[i]-x[j] >= 2) \/ (x[j]-x[i] >= 2));

constraint forall (t in 0..horizon, i in 1..n) (
  let {
    var bool: active = (x[i] <= t /\ t<x[i]+d)
  } in
  ( active <-> R[t,i] = m[i] ) /\
  ( (not active) <-> R[t,i] = 0 )
);

constraint forall (t in 0..horizon) (sum (i in 1..n) (R[t,i]) <= maxR);

solve satisfy;

output [show(x),"\n",concat([show([R[t,i] | i in 1..n])++"\n"|t in 0..horizon])];
