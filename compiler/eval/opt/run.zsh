#!/usr/bin/env zsh
set -e
ZOKRATES=$(which zokrates)
CIRCIFY=$(which compiler-exe)
SCRIPT_PATH="${0:A:h}"


#  1 argument: array size
#  2 argument: optimized?
#  3 argument: number of constraints
#  4 argument: z3 time
function save_result() {
    echo $1,$2,$3,$4 >> $SCRIPT_PATH/results-wip.csv
}

function init_results() {
    rm -rf $SCRIPT_PATH/results-wip.csv
    echo rounds,optimized,constraints,z3_time > $SCRIPT_PATH/results-wip.csv
}

function commit_results() {
    mv $SCRIPT_PATH/results-wip.csv $SCRIPT_PATH/results.csv
}

# First argument: size
# Second argument: optimized?
function count() {
    echo "rounds: $1, optimized: $2"
    d=$(mktemp -d -p . )
    cd $d
    touch C
    cfile=$(readlink -f "C")
    sed "s/LEN/$1/" ../lfsc.c > tmp.c
    if [[ $2 = "True" ]]; then
        z3t=$(env C_streams=time::z3 C_smt_cfold_in_sub=False C_smt_opts=ee,mem,flattenAnds,cfee C_c_sv=True C_opt_z3=$2 $CIRCIFY -C $cfile c-check compute tmp.c | rg Time | awk '{ print $3 }')
        env C_smt_cfold_in_sub=False C_smt_opts=ee,mem,flattenAnds,cfee C_c_sv=True $CIRCIFY -C $cfile c-check-emit-r1cs compute tmp.c
    else
        z3t=$(env C_streams=time::z3 C_smt_cfold_in_sub=False C_smt_opts=ee,mem,flattenAnds C_c_sv=True C_opt_z3=$2 $CIRCIFY -C $cfile c-check compute tmp.c | rg Time | awk '{ print $3 }')
        env C_smt_cfold_in_sub=False C_smt_opts=ee,mem,flattenAnds C_c_sv=True $CIRCIFY -C $cfile c-check-emit-r1cs compute tmp.c
    fi
    n=$(head -n 1 $cfile | awk '{print $3}')
    cd -
    echo Time: $z3t N: $n
    save_result $1 $2 $n $z3t
    rm -rf $d
}

init_results
for s in $(seq 10 5 30); do
    for o in True False; do
        count $s $o
    done
done
commit_results

