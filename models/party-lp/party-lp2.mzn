int : ng;
int : nh;
int : p;
int : nguests = ng;
set of int : GUESTS = 1..nguests;
int : nhosts = nh;
set of int : HOSTS = 1..nhosts;
int : nperiods = p;
set of int : PERIODS = 1..nperiods;

array [GUESTS] of int : crew;
array [GUESTS] of int : c = crew;
array [HOSTS] of int : spareCapacity;
array [HOSTS] of int : K = spareCapacity;

array [HOSTS, GUESTS, PERIODS] of var 0..1 : gamma;

solve satisfy;

% Boats don't overflow with guests.
constraint forall (i in HOSTS, t in PERIODS) (
  (sum(k in GUESTS)(c[k]*gamma[i,k,t])) <= K[i]
);

% forall t:
%   bin_packing_capa(K, gamma_3a[_,t], c)

% Each guest visits exactly one host in every period.
constraint forall (k in GUESTS, t in PERIODS) (
  (sum(i in HOSTS)(gamma[i,k,t])) = 1
);

% Each guest visits a host at most once.
constraint forall (i in HOSTS, k in GUESTS) (
  (sum(t in PERIODS)(gamma[i,k,t])) <= 1
);

% % Guests meet at most once.
% constraint forall (i,j in HOSTS, k,l in GUESTS, t,s in PERIODS where i != j /\
%                                                                      k < l  /\
%                                                                      s != t) (
%   gamma[i,k,t] + gamma[i,l,t] + gamma[j,k,s] + gamma[j,l,s] <= 3
% );

% output ["Hosts:" ++ concat([if fix(delta[i])=1 then " " ++ show(i) else "" endif | i in BOATS]) ++ "\n"]++
%   [ "period " ++ show(t) ++ ":\n" ++ 
%   concat([ if fix(gamma[i,k,t])=1 then show(k) ++ " on " ++ show(i) ++ "\n" else "" endif | i,k in BOATS ]) | t in PERIODS ]
% ;

% array [index_set_2of3(gamma),index_set_3of3(gamma)] of var index_set_1of3(gamma): gamma_3a;
% constraint channelCAB(gamma_3a, gamma);
