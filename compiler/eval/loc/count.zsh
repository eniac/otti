#!/usr/bin/env zsh

typeset -A components
components=(
    c "../../src/Codegen/C"
    circom "../../src/Codegen/Circom"
    z3 "../../src/Targets/SMT/Z3.hs"
    r1cs "../../src/Targets.R1cs.Main.hs ../../src/Targets.R1cs.Main/ ../../src/IR/SMT/ToPf.hs"
    core "../../src/IR/SMT ../../src/Codegen/FrontEnd.hs ../../src/Codegen/Circify.hs ../../src/Codegen/Circify"
    zokrates "../../src/Codegen/Zokrates"
    zokrates-all "../../src/Codegen/Zokrates ../../src/AST/Zokrates.hs ../../src/Parser/Zokrates.hs ../../src/Parser/Zokrates"
)

for c in ${(k)components}
do
    n=$(tokei -o json "${(s: :)components[$c]}" | jq .Haskell.code)
    echo "$c: $n"
done


