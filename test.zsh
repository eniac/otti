#!/usr/bin/env zsh
stack run -- setup --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main
stack run -- prove --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main -x ./test/Code/Circom/bitify4.x -w ./test/Code/Circom/bitify4.w
stack run -- verify --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main -x ./test/Code/Circom/bitify4.x

