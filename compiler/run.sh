#!/bin/bash
set -x
for file in $(ls ~/git/lp-fuzzer/mpc/cfiles); do
        C_outputs=$file.zkif stack run -- c main ~/git/lp-fuzzer/mpc/cfiles/$file --emit-r1cs
        C_outputs=$file.zkif stack run -- c main ~/git/lp-fuzzer/mpc/cfiles/$file --prove -i <(echo "")
        mv $file.inp.zkif zkif/"$file.inp.zkif" && mv $file.wit.zkif zkif/"$file.wit.zkif" && mv $file.zkif zkif/"$file.zkif"
        ls zkif/
done
