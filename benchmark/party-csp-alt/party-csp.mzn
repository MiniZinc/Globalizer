include "all_different.mzn";

int : periods;
int : nhosts;
int : nguests;

array [1..nguests] of int : crew;
array [1..nhosts] of int : spareCapacity;

array [1..nguests, 1..periods] of var 1..nhosts : h;
array [1..nguests, 1..nhosts, 1..periods] of var 0..1 : v;
array [1..nguests, 1..nguests, 1..periods] of var 0..1 : m;

constraint forall (i in 1..nguests) (alldifferent([h[i,t] | t in 1..periods]));

constraint forall (i in 1..nguests, j in 1..nhosts, t in 1..periods)
  (v[i,j,t] = 1 <-> h[i,t]=j);

constraint forall (j in 1..nhosts, t in 1..periods)
  (sum (i in 1..nguests) (crew[i]*v[i,j,t]) <= spareCapacity[j]);

constraint forall (k in 1..nguests, l in 1..nguests, t in 1..periods where k<l)
  (h[k,t] = h[l,t] -> m[k,l,t] = 1);

constraint forall (k in 1..nguests, l in 1..nguests where k<l)
  (sum (t in 1..periods) (m[k,l,t]) <= 1);

solve satisfy;

output [show(h)];
