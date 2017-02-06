int : items = 5;
int : bins = 3;
array [1..items] of int : weights = [1,2,1,2,1];
array [1..bins] of int : capacities = [2,2,3];

array[1..items] of var 1..bins : x;
array[1..items,1..bins] of var 0..1 : y;

constraint forall (i in 1..items, b in 1..bins)
  (y[i,b]=1 <-> x[i]=b);

constraint forall (b in 1..bins)
  (capacities[b] >= sum ( i in index_set(x) ) ( weights[i] * y[i,b]));

solve satisfy;

output [show(x)];
