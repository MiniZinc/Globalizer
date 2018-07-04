int: S = 3;
int: N = S*S;
array[1..N, 1..N] of int: givens;

array[1..N, 1..N, 1..N] of var 0..1: x;
% array[1..N, 1..N] of var 1..9: p;

constraint forall(i,j in 1..N where
                    givens[i,j] != 0) (
    x[i,j, givens[i,j]] = 1
);

constraint forall(i in 1..9, j in 1..9) (sum([x[i,j,k] | k in 1..9]) = 1);
constraint forall(i in 1..9, k in 1..9) (sum([x[i,j,k] | j in 1..9]) = 1);
constraint forall(j in 1..9, k in 1..9) (sum([x[i,j,k] | i in 1..9]) = 1);
constraint forall(I,J in 0..2,
                    k in 1..9) ( sum([x[i,j,k] | i in (I*3+1)..(I*3+3),
                                                 j in (J*3+1)..(J*3+3)]) = 1);
% constraint forall(i in 1..9, k in 1..9, j in 1..9) (p[i,j] = k <-> x[i,j,k] = 1);


solve satisfy;

%output [ "sudoku:\n" ] ++
%    [ show(p[i,j]) ++
%        if j = N then
%            if i mod S = 0 /\ i < N then "\n\n" else "\n" endif
%        else
%            if j mod S = 0 then "  " else " " endif
%        endif
%    | i,j in 1..N ];
