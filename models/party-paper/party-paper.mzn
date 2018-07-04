int: p; int: nh; int: ng;
set of int : HostBoats = 1..nh;
set of int : GuestCrews = 1..ng;
set of int : Time = 1..p;

array [GuestCrews] of int : crew;
array [HostBoats]  of int : capacity;

array [GuestCrews, Time] of var HostBoats : hostedBy;

array [GuestCrews, HostBoats, Time] of var 0..1 : visits;
constraint forall (g in GuestCrews, h in HostBoats, t in Time)
  (visits[g,h,t] = 1 <-> hostedBy[g,t]=h);

constraint forall (h in HostBoats) (
    forall (g in GuestCrews)
      (sum (t in Time) (visits[g,h,t]) <= 1)
/\  forall (t in Time)
      (sum (g in GuestCrews) (crew[g]*visits[g,h,t]) <= capacity[h])
);

array [GuestCrews, GuestCrews, Time] of var 0..1 : meet;
constraint forall (k, l in GuestCrews where k<l) (
    forall (t in Time)
      (hostedBy[k,t] = hostedBy[l,t] -> meet[k,l,t] = 1)
/\  sum (t in Time) (meet[k,l,t]) <= 1
);

solve satisfy;
output [show(hostedBy), show(visits), show(meet)];
