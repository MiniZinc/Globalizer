include "all_different.mzn";
annotation booleanises(array[int,int] of var int:x, int:i);

int : p;    % number of periods
int : nh;   % number of hosts
int : ng;   % number of guests

% size of crew per guest boat
array [1..ng] of int : crew;
% guest capacity per host boat
array [1..nh] of int : spareCapacity;

% h[i,t]=j if guest boat i visits host boat j at time t
array [1..ng, 1..p] of var 1..nh : h;
% v[i,j,t]=1 if guest boat i visits host boat j at time t 
array [1..ng, 1..nh, 1..p] of var 0..1 : v :: booleanises(h,2);

% Connect v and h variables.
constraint forall (t in 1..p, i in 1..ng, j in 1..nh)
  (v[i,j,t] = 1 <-> h[i,t]=j);

% A host boat must not exceed its capacity.
constraint forall (t in 1..p, j in 1..nh)
  (sum (i in 1..ng) (crew[i]*v[i,j,t]) <= spareCapacity[j]);

solve satisfy;

output [show(h), show(v) ];



crew=[2,3, 6,2,2,4,2,4,5,4];
spareCapacity=[4,6,10,10,8];


nh = 5;
ng = 10;
p = 4;
