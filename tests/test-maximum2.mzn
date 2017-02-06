include "globals.mzn";

int : cols = 3;
int : n = 4;
int : rows = 2;
array [1..rows,1..cols] of var 1..n : x;
array [1..rows] of var 1..n : m;

constraint forall (r in 1..rows) (forall (i in 1..cols) (x[r,i] <= m[r]));
constraint forall (s in 1..rows) (exists (j in 1..cols) (x[s,j] == m[s]));


% % is equivalent to:
% constraint forall (r in 1..rows)
%    (forall (i in 1..cols) (x[r,i] <= m[r])
%     /\
%     let {int:s = r} in exists (j in 1..cols) (x[s,j] = m[s]));

% is equivalent to:
% constraint forall (r in 1..rows)
%    (forall (i in 1..cols) (x[r,i] <= m[r]) /\ exists (j in 1..cols) (x[r,j] = m[r]));

% constraint forall (r in 1..rows)
%    (forall (i in 1..cols) (maximum(m[r], [x[r,j] | j in 1..cols])));

% % which is:
% constraint forall (r in 1..rows)
%   ( maximum(m[r], x[r,1..cols]) );

solve satisfy;

output [show(x), show(m)];
