% RUNS ON mzn_mer_fd
% RUNS ON mzn_mer_lazyfd
% RUNS ON mzn_mer_lp
% RUNS ON zinc_fdic_mznlib
% RUNS ON zinc_mip_hl
% RUNS ON minizinc_fd
% RUNS ON minizinc_cpx
%-----------------------------------------------------------------------------%
% Example from the MiniZinc paper:
% (square) job shop scheduling in MiniZinc
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Instance

size = 2;
%d = [| 2,5
%     | 3,4 |];
%array[int] of int : d_1 = [2,3];
%array[int] of int : d_2 = [5,4];

%-----------------------------------------------------------------------------%
% Model

int: size;                                  % size of problem
array [1..size,1..size] of int: d;          % task durations
int: total = sum(i,j in 1..size) (d[i,j]);  % total duration
%int: total = 14;
array [1..size,1..size] of var 0..total: s; % start times
var 0..total: end;                          % total end time

% predicate no_overlap(var int:s1, int:d1, var int:s2, int:d2) =
%     s1 + d1 <= s2 \/ s2 + d2 <= s1;

constraint end != end;

constraint
    forall(machine in 1..size) (
        % precedences
        forall(j in 1..size-1) (s[machine,j] + d[machine,j] <= s[machine,j+1]) /\

        % makespan
        s[machine,size] + d[machine,size] <= end /\

        % on one machine, tasks can't overlap
        forall(j,k in 1..size where j < k) (
            %no_overlap(s[j,machine], d[j,machine], s[k,machine], d[k,machine])
	    s[j,machine]+d[j,machine] <= s[k,machine]  \/
            s[k,machine]+d[k,machine] <= s[j,machine]
        )
    );

solve minimize end;

output [
  "jobshop2x2\n",
  "s[1..2, 1..2] = [", show(s[1, 1]), " ", show(s[1, 2]), "\n",
  "                 ", show(s[2, 1]), " ", show(s[2, 2]), "]\n"
];

%-----------------------------------------------------------------------------%
