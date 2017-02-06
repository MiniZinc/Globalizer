int : m = 4;

array[1..m] of var 0..1 : b;
%var 1..4 : x;

%constraint forall (i in 1..m) (x = i <-> b[i] = 1);

constraint sum(b) = 1;

solve satisfy;

output [%show(x), 
       " ", show(b)];

