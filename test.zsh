#!/usr/bin/env zsh
# Runs end-to-end workflow tests.
set -ex
stack run -- setup --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main
stack run -- prove --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main -i ./test/Code/Circom/bitify4.in
stack run -- verify --circom ./test/Code/Circom/bitify4.circom --libsnark libsnark-frontend/build/src/main

stack run -- c-setup outer ./test/Code/C/fn_call.c
stack run -- -i <(echo x 1; echo y 4;) c-prove outer ./test/Code/C/fn_call.c
stack run -- verify

stack run -- c-setup bar ./test/Code/C/struct_inout.c
stack run -- -i <(echo p.x 1; echo p.y 4;) c-prove bar ./test/Code/C/struct_inout.c
stack run -- verify

C_no_overflow=True stack run -- c-setup outer ./test/Code/C/fn_call.c
C_no_overflow=True stack run -- -i <(echo x 1; echo y 4;) c-prove outer ./test/Code/C/fn_call.c
C_no_overflow=True stack run -- verify

stack run -- c-check-setup outer ./test/Code/C/fn_call.c
stack run -- c-check-prove outer ./test/Code/C/fn_call.c
stack run -- verify

stack run -- c-check-setup bar ./test/Code/C/struct_inout.c
stack run -- c-check-prove bar ./test/Code/C/struct_inout.c
stack run -- verify

C_c_sv=True stack run -- c-check-setup main ./test/Code/C/sv/assume.c
C_c_sv=True stack run -- c-check-prove main ./test/Code/C/sv/assume.c
C_c_sv=True stack run -- verify
