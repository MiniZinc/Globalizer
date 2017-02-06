int: n;                                  % number of boats
int: p;                                  % number of periods
array[1..n] of int: cap;                 % cap[i] is the capacity of boat i
array[1..n] of int: crew;                % crew[i] is the crew size of boat i
array[1..n] of int: guestCap;
array[1..n] of var 0..1: isHost;         % isHost[i]=1 iff boat i is a host boat
array[1..n,1..n,1..p] of var 0..1: isGuest;  % isGuest[i,k,t]=1 iff boat k is guest of boat i in period t
var 1..n: objective;                     % number of host boats

% Channel.
% location[i,t]=j if crew i is on boat j at time t
array[1..n,1..p] of var 1..n : location;
constraint forall (i in 1..n, t in 1..p, j in 1..n)
    (location[i,t]=j <-> (    (i = j /\ isHost[i]=1)
                           \/ (isGuest[j,i,t] = 1) ) );

% A boat can only be visited if it is a host
constraint forall (i,k in 1..n, t in 1..p) (isGuest[i,k,t]-isHost[i] <= 0);

% The capacity of a host boat cannot be exceeded
constraint
  forall (t in 1..p)
    (forall (h in 1..n)
      (sum(k in 1..n where k!=h) (crew[k]*isGuest[h,k,t]) <= guestCap[h]));

% Each guest crew must have a host
constraint forall (k in 1..n, t in 1..p)
                   (sum(i in 1..n where k!=i) (isGuest[i,k,t]) + isHost[k] = 1);

% Each guest crew cannot visit a host most than once.
constraint forall (i,k in 1..n)
                   (sum(t in 1..p) (isGuest[i,k,t]) <= 1);

% Any pair of guests can meet at most once.
constraint forall (i,j,k,l in 1..n, t,s in 1..p where i!=j/\i!=k/\i!=l/\k<l/\k!=j/\j!=l/\s!=t )
                   (isGuest[i,k,t] + isGuest[i,l,t] + isGuest[j,k,s] + isGuest[j,l,s] <= 3);

% Objective function
constraint sum(i in 1..n) (isHost[i]) = objective;

solve minimize objective;
