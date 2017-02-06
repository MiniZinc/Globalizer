int: n;                                  % number of boats
int: p;                                  % number of periods
array[1..n] of int: cap;                 % cap[i] is the capacity of boat i
array[1..n] of int: crew;                % crew[i] is the crew size of boat i
array[1..n] of var 0..1: isHost;         % isHost[i]=1 iff boat i is a host boat
array[1..n,1..n,1..p] of var 0..1: isGuest;  % isGuest[i,k,t]=1 iff boat k is guest of boat i in period t
var 1..n: objective;                     % number of host boats
array [1..n,1..n,1..n,1..p] of var 0..1: meet;

% CD
% A boat can only be visited if it is a host
constraint forall (i,k in 1..n, t in 1..p) (isGuest[i,k,t]-isHost[i] <= 0);

% CCAP
% The capacity of a host boat cannot be exceeded
constraint forall (i in 1..n, t in 1..p)
                   (sum(k in 1..n where k!=i) (crew[k]*isGuest[i,k,t]) <= cap[i]-crew[i]);

% GA
% Each guest crew must have a host
constraint forall (k in 1..n, t in 1..p)
                   (sum(i in 1..n where k!=i) (isGuest[i,k,t]) + isHost[k] = 1);

% GB
% Each guest crew cannot visit a host most than once.
constraint forall (i,k in 1..n)
                   (sum(t in 1..p) (isGuest[i,k,t]) <= 1);

% % W
% % Any pair of guests can meet at most once.
% constraint forall (i,j,k,l in 1..n, t,s in 1..p where i!=j/\i!=k/\i!=l/\k<l/\k!=j/\j!=l/\s!=t )
%                    (isGuest[i,k,t] + isGuest[i,l,t] + isGuest[j,k,s] + isGuest[j,l,s] <= 3);

% S
constraint forall (i,k,l in 1..n, t in 1..p where k<l /\ i != k /\ i != l)
  (2*meet[i,k,l,t] - isGuest[i,k,t] - isGuest[i,l,t] <= 0);
% V
constraint forall (i,k,l in 1..n, t in 1..p where k<l /\ i != k /\ i != l)
  (isGuest[i,k,t] + isGuest[i,l,t] - meet[i,k,l,t] <= 1);
% Y
% constraint forall (i,k in 1..n)
%   (sum (t in 1..p, l in 1..n where k<l) (meet[i,k,l,t]) <= 1);
constraint forall (k,l in 1..n where k<l)
  (sum (t in 1..p, i in 1..n) (meet[i,k,l,t]) <= 1);

% Objective function
constraint sum(i in 1..n) (isHost[i]) = objective;

solve minimize objective;

output [show(objective)];
