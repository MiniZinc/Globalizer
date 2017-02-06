#!/bin/sh

BENCHMARK=$1

MZN=benchmark/$1/*mzn
DATA=benchmark/$1/*dzn
BASE=$(basename $MZN)
STRIPPED=${BASE%%.mzn}

MADIR=$(pwd)

trap "echo Exited!; exit;" SIGINT SIGTERM

../subproblemsplitter/SubproblemSplitter -I /opt/minizinc/minizinc-1.6/lib/minizinc/std $MZN $DATA
ls generated_subproblems/$STRIPPED/

SUBMODELS=$(ls generated_subproblems/$STRIPPED/* | sed "s/d.\.mzn//" | uniq)

for i in $SUBMODELS ; do
    echo "SUBMODEL $i"
    ./GroupOutput "$i"*
done
