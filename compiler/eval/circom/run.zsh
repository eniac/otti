set -ex
CIRCOM=$(which circom)
CIRCIFY=$(which compiler-exe)
SCRIPT_PATH="${0:A:h}"


# First argument: benchmark name
# Second argument: circom path
# Third argument: compiler
# Fourth argument: number of constraints
# Fifth argument: wall time
function save_result() {
    echo $1,$2,$3,$4,$5 >> $SCRIPT_PATH/results-wip.csv
}

function init_results() {
    rm -rf $SCRIPT_PATH/results-wip.csv
    echo benchmark,path,compiler,constraints,wall_time > $SCRIPT_PATH/results-wip.csv
}

function commit_results() {
    mv $SCRIPT_PATH/results-wip.csv $SCRIPT_PATH/results.csv
}

# First argument: circom circuit path
function circify_count() {
    d=$(mktemp -d -p . )
    cd $d
    s=$(date +%s.%N)
    e=$(date +%s.%N)
    cd -
    save_result $1 circify $n $(($e- $s))
    rm -rf $d
}

# First argument: circom circuit path
function circom_count() {
    d=$(mktemp -d -p . )
    cd $d
    s=$(date +%s.%N)
    $CIRCOM -o circuit.json $1
    n=$(jq '.constraints|length' circuit.json)
    e=$(date +%s.%N)
    cd -
    rm -rf $d
}

# First argument: circom path
# Second argument: compiler
# Third argument: benchmark name
function count() {
    d=$(mktemp -d -p . )
    cd $d
    s=$(date +%s.%N)
    case $2 in
    circom)
        $CIRCOM -o circuit.json $1
        n=$(jq '.constraints|length' circuit.json)
        ;;
    circify)
        $CIRCIFY --circom $1 -C C emit-r1cs
        n=$(head -n 1 C | awk '{print $3}')
        ;;
    *)
        echo "Unknown circom compiler: $2"
        exit 1
        ;;
    esac
    e=$(date +%s.%N)
    cd -
    save_result $3 $1 $2 $n $(($e - $s))
    rm -rf $d
}
typeset -A assoc_array
assoc_array=(poseidon
             ~/repos/llcl/circomlib/test/circuits/poseidon_test.circom
             mimc
             ~/repos/llcl/circomlib/test/circuits/mimc_test.circom
             eddsa
             ~/repos/llcl/circomlib/test/circuits/eddsaposeidon_test.circom
             binsub
             ~/repos/llcl/circomlib/test/circuits/binsub_test.circom
             mux4
             ~/repos/llcl/circomlib/test/circuits/mux4_1.circom
             ec-scalar-mul
             ~/repos/llcl/circomlib/test/circuits/escalarmulfix_test.circom
             pedersen
             ~/repos/llcl/circomlib/test/circuits/pedersen2_test.circom
             sha256_2
             ~/repos/llcl/circomlib/test/circuits/sha256_2_test.circom)

init_results
for b in "${(@k)assoc_array}"; do
    p="${assoc_array[$b]}"
    for compiler in circify circom; do
        count $p $compiler $b
    done
done
commit_results

