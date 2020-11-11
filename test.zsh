#!/usr/bin/env zsh
# Runs end-to-end workflow tests.
set -ex

function num_constraints() {
  cat C | head -n 1 | awk '{ print $3 }'
}

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

stack run -- c-setup sum ./test/Code/C/array_8.c
stack run -- -i ./test/Code/C/inputs/array_8_sum.in c-prove sum ./test/Code/C/array_8.c
stack run -- verify

stack run -- c-setup struct_sum ./test/Code/C/array_8.c
C_smt_check_opts=True stack run -- -i ./test/Code/C/inputs/array_8_struct_sum.in c-prove struct_sum ./test/Code/C/array_8.c
stack run -- verify

C_pequin_io=True C_no_overflow=True stack run -- c-setup compute ./test/Code/C/pequin/mm_flat.c
C_pequin_io=True C_no_overflow=True C_loop_bound=3 stack run -- -i ./test/Code/C/pequin/inputs/mm_flat.i c-prove compute ./test/Code/C/pequin/mm_flat.c
stack run -- verify
[[ $(num_constraints) = 27 ]]

C_smt_benes_thresh=1 C_pequin_io=True stack run -- c-setup compute ./test/Code/C/pequin/ptrchase_8_8.c
C_smt_benes_thresh=1 C_pequin_io=True stack run -- -i ./test/Code/C/pequin/inputs/ptrchase_8_8.i c-prove compute ./test/Code/C/pequin/ptrchase_8_8.c
stack run -- verify

C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- c-setup flex test/Code/C/benes_arrays.c
C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- -i <(echo x 1; echo y 1) c-prove flex test/Code/C/benes_arrays.c
stack run -- verify

C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- c-setup flex test/Code/C/benes_arrays.c
C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- -i <(echo x 0; echo y 0) c-prove flex test/Code/C/benes_arrays.c
stack run -- verify

C_smt_benes_thresh=1 C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- c-setup flex test/Code/C/benes_arrays.c
C_smt_benes_thresh=1 C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- -i <(echo x 1; echo y 1) c-prove flex test/Code/C/benes_arrays.c
stack run -- verify
