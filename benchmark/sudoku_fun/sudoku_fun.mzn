int: S = 3;
int: N = S*S;
array[1..N, 1..N] of int: givens;
array[1..N, 1..N] of var 1..9: p;

constraint forall(i in 1..N, j in 1..N) (
    if givens[i,j] != 0 then p[i,j] = givens[i,j] else true endif
);

constraint forall(r1 in 1..N, c1 in 1..N, r2 in 1..N, c2 in 1..N ) (
    if ((not (r1 = r2 /\ c1 = c2)) /\
       ((r1 = r2)
     \/ (c1 = c2)
     \/ (   ((r1-1) div S = (r2-1) div S)
         /\ ((c1-1) div S = (c2-1) div S))))
    then
        p[r1,c1] != p[r2,c2]
    else
        true
    endif
);

solve satisfy;

output [ "sudoku:\n" ] ++
    [ show(p[i,j]) ++
        if j = N then
            if i mod S = 0 /\ i < N then "\n\n" else "\n" endif
        else
            if j mod S = 0 then "  " else " " endif
        endif
    | i,j in 1..N ];
