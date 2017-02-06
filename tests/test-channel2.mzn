int : p;    % number of periods
int : nh;   % number of hosts
int : ng;   % number of guests

p = 4;
nh = 3;
ng = 5;

% h[i,t]=j if guest boat i visits host boat j at time t
array [1..ng, 1..p] of var 1..nh : h;
% v[i,j,t]=1 if guest boat i visits host boat j at time t 
array [1..ng, 1..nh, 1..p] of var 0..1 : v;

% % Connect v and h variables.
% constraint forall (t in 1..p, i in 1..ng, j in 1..nh)
%   (v[i,j,t] = 1 <-> h[i,t]=j);

constraint forall (t in 1..p, i in 1..ng)
  (forall (j in 1..nh)
    (v[i,j,t] = 1 <-> h[i,t]=j));

% % desired result:
% constraint forall (t in 1..p, i in 1..ng)
%   (channel(h[i,t], v[i,_,t]));

solve satisfy;
