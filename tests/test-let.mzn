var 0..10 : x;
constraint let { var 0..10 : y } in x+y=5;
solve satisfy;
output [show(x)];
