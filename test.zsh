#!/usr/bin/env zsh
set -ex
stack run -- setup --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main
stack run -- prove --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main -i ./test/Code/Circom/bitify4.in
stack run -- verify --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main

stack run -- setup --opt --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main
stack run -- prove --opt --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main -i ./test/Code/Circom/bitify4.in
stack run -- verify --opt --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main
