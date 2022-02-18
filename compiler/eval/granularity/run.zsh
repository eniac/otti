#!/usr/bin/env zsh

set -ex

SCRIPT_PATH="${0:A:h}"
CIRCIFY=$(which compiler-exe)

# 0: perm size
# 1: n perms
# 2: largest perm
# 3: z3 time
# 4: number of constraints
function save_result() {
    echo $1,$2,$3,$4,$5 >> $SCRIPT_PATH/results-wip.csv
}

function init_results() {
    rm -rf $SCRIPT_PATH/results-wip.csv
    echo size,perms,largest_perm,z3_time,constraints > $SCRIPT_PATH/results-wip.csv
}

function commit_results() {
    mv $SCRIPT_PATH/results-wip.csv $SCRIPT_PATH/results.csv
}

init_results
perms=6
for size in 1 2 4 8; do
    for i in $(seq 1 $perms); do
        d=$(mktemp -d -p . )
        cd $d
        if [[ $i -lt $perms ]]; then
            s=$(echo -n $i; printf " 1"%.0s {1..$(($perms - $i))})
        else
            s=$i
        fi
        ../gen.py -w $size ${=s} > out.c
        z3t=$(C_streams=time::z3 C_c_sv=True $CIRCIFY c-check perm out.c | rg Time | awk '{print $3}')
        echo $size : $s
        C_smt_benes_thresh=1000 C_c_sv=True $CIRCIFY c-emit-r1cs perm out.c
        n=$(head -n 1 C | awk '{print $3}')
        echo $z3t $n
        save_result $size $perms $i $z3t $n
        cd -
        rm -rf $d
    done
done
commit_results
