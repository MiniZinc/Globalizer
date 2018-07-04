include "all_different.mzn";

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
array [1..ng, 1..nh, 1..p] of var 0..1 : v;
% m[i,j,t]=1 if guest boats i and j meet at time t
array [1..ng, 1..ng, 1..p] of var 0..1 : m;

% A guest boat must visit a host at most once.
constraint forall (i in 1..ng)
  (alldifferent([h[i,t] | t in 1..p]));

% Connect v and h variables.
constraint forall (t in 1..p, i in 1..ng, j in 1..nh)
  (v[i,j,t] = 1 <-> h[i,t]=j);

% A host boat must not exceed its capacity.
constraint forall (t in 1..p, j in 1..nh)
  (sum (i in 1..ng) (crew[i]*v[i,j,t]) <= spareCapacity[j]);

% Two guest boats meet if they are on the same host boat.
constraint forall (k in 1..ng, l in 1..ng, t in 1..p where k<l)
  (h[k,t] = h[l,t] -> m[k,l,t] = 1);

% Two guest boats meet at most once.
constraint forall (k in 1..ng, l in 1..ng where k<l)
  (sum (t in 1..p) (m[k,l,t]) <= 1);

solve satisfy;

output [show(h)];
