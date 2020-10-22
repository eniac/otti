#!/usr/bin/env zsh

typeset -A components
components=(
    c "../../src/Codegen/C ../../src/Codegen/C.hs"
    circom "../../src/Codegen/Circom"
    z3 "../../src/Targets/SMT/TySmtToZ3.hs"
    r1cs "../../src/IR/R1cs.hs ../../src/IR/R1cs/ ../../src/IR/SMT/ToPf.hs"
)

for c in ${(k)components}
do
    n=$(tokei -o json "${(s: :)components[$c]}" | jq .Haskell.code)
    echo "$c: $n"
done


