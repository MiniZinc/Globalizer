array[1..10] of var 0..10 : x;

constraint forall (i in 1..10) (x[i] = i `div` 2);
% constraint forall (i in 1..10) (x[i] = div(i, 2));

% constraint forall (i in 1..10) (x[i] = i div 2);
% constraint forall (i in 1..10) (x[i] = 'div'(i, 2));

solve satisfy;

output [show(x)];
