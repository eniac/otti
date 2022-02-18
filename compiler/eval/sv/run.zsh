#!/usr/bin/env zsh

set -e
CIRCIFY=$(which compiler-exe)
SCRIPT_PATH="${0:A:h}"

typeset -A paths
paths=(sadd-overflow
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/AdditionIntMax.c
    sadd-underflow
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/AdditionIntMin.c
    conversion
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/ConversionToSignedInt.c
    division
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/Division-1.c
    int-promotion
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/IntegerPromotion-1.c
    multiplication-1
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/Multiplication-1.c
    multiplication-2
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/Multiplication-2.c
    no-conversion
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/NoConversion.c
    no-neg-int-const
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/NoNegativeIntegerConstant.c
    post-dec
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/PostfixDecrement.c
    pre-dec
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/PrefixDecrement.c
    post-inc
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/PostfixIncrement.c
    pre-inc
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/PrefixIncrement.c
    un-minus
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/UnaryMinus.c
    conversions
    ~/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/UsualArithmeticConversions.c
)
typeset -A results
results=(sadd-overflow bad
    sadd-underflow bad
    conversion good
    division bad
    int-promotion good
    multiplication-1 good
    multiplication-2 bad
    no-conversion bad
    no-neg-int-const good
    post-dec bad
    post-inc bad
    pre-dec bad
    pre-inc bad
    un-minus bad
    conversions good
)

for b in "${(@k)paths}"; do
    p="${paths[$b]}"
    expected_result="${results[$b]}"
    output=$($CIRCIFY c-check main $p)
    case $expected_result in
    good)
        if [[ "$output" =~ "No bug!" ]]
        then
        else
            echo "Incorrect result for $b"
            echo "at $p"
            echo "Output: $output"
        fi
        ;;
    bad)
        if [[ "$output" =~ "Bug!" ]]
        then
        else
            echo "Incorrect result for $b"
            echo "at $p"
            echo "Output: $output"
        fi
        ;;
    *)
        echo "Bad expected_result for $b: $expected_result"
        exit 1
        ;;
    esac
done


