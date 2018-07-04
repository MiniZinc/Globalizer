int : n = 3;
array[1..n] of var int: x;
array[1..n] of var int: y;
array[1..n] of var int: pack_s;

constraint forall ( i in 1..n-1, j in i+1..n ) ( 
    x[j]-x[i]>=pack_s[i] \/ x[i]-x[j]>=pack_s[j] \/ y[j]-y[i]>=pack_s[i] \/ 
    y[i]-y[j]>=pack_s[j] 
);
