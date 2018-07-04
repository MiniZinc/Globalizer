% cars.mzn.model
% vim: ft=zinc ts=4 sw=4 et tw=0
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Thu Oct 25 15:09:27 EST 2007
%
% Car sequencing problem taken from
%
% See http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob001/spec.html

include "globals.mzn";

int: n_cars;
int: n_options;
int: n_classes;

array [1..n_options] of int: option_max_per_block;
array [1..n_options] of int: option_block_size;
array [1..n_classes] of int: cars_in_class;
array [1..n_classes, 1..n_options] of 0..1: class_option_need;

    % Decision variable: the class of car being started at each step.
    %
array [1..n_cars] of var 1..n_classes: step_class;

    % An auxiliary decision variable: which options are required by the
    % car started at each step.
    %
array [1..n_cars, 1..n_options] of var 0..1: step_option_use;

constraint
    forall (s in 1..n_cars, p in 1..n_options) (
        step_option_use[s, p] = class_option_need[step_class[s], p]
    );

% Require that for each option p with block size b and maximum limit m, no
% consecutive sequence of b cars contains more than m that require option p.
%
constraint
    forall (p in 1..n_options) (
      forall (i in 1..(n_cars - (option_block_size[p] - 1))) (
        sum (j in 0..(option_block_size[p] - 1)) (step_option_use[i + j, p])
            <= option_max_per_block[p]
    ) );

% Require that the right number of cars in each class are produced.
%
constraint
    forall (c in 1..n_classes) (count(step_class, c, cars_in_class[c]));

% Find any solution.
%
solve satisfy;

output [
   "step_class = ", show(step_class), "\n",
   "step_option_use = ", show(step_option_use), "\n"
];

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
