#!/usr/bin/env zsh
# Runs end-to-end workflow tests.
set -ex

function num_constraints() {
  cat C | head -n 1 | awk '{ print $3 }'
}

stack run -- circom ./test/Code/Circom/bitify4.circom --setup
stack run -- circom ./test/Code/Circom/bitify4.circom -i ./test/Code/Circom/bitify4.in --prove
stack run -- verify

stack run -- c outer ./test/Code/C/fn_call.c --setup
stack run -- c outer ./test/Code/C/fn_call.c --prove -i <(echo x 1; echo y 3)
stack run -- verify

stack run -- c bar ./test/Code/C/struct_inout.c --setup
stack run -- c bar ./test/Code/C/struct_inout.c --prove -i <(echo p.x 1; echo p.y 4;)
stack run -- verify

C_no_overflow=True stack run -- c outer ./test/Code/C/fn_call.c --setup
C_no_overflow=True stack run -- c outer ./test/Code/C/fn_call.c --prove -i <(echo x 1; echo y 3)
stack run -- verify

stack run -- c outer ./test/Code/C/fn_call.c --check --setup
stack run -- c-check-prove outer ./test/Code/C/fn_call.c
stack run -- verify

stack run -- c bar ./test/Code/C/struct_inout.c --check --setup
stack run -- c-check-prove bar ./test/Code/C/struct_inout.c
stack run -- verify

C_c_sv=True stack run -- c main ./test/Code/C/sv/assume.c --check --setup
C_c_sv=True stack run -- c-check-prove main ./test/Code/C/sv/assume.c
stack run -- verify

stack run -- c sum ./test/Code/C/array_8.c --setup
stack run -- c sum ./test/Code/C/array_8.c --prove -i ./test/Code/C/inputs/array_8_sum.in
stack run -- verify

stack run -- c struct_sum ./test/Code/C/array_8.c --setup
stack run -- c struct_sum ./test/Code/C/array_8.c --prove -i ./test/Code/C/inputs/array_8_struct_sum.in
stack run -- verify

C_pequin_io=True C_no_overflow=True stack run -- c compute ./test/Code/C/pequin/mm_flat.c --setup
C_pequin_io=True C_no_overflow=True stack run -- c compute ./test/Code/C/pequin/mm_flat.c --prove -i ./test/Code/C/pequin/inputs/mm_flat.i
stack run -- verify
[[ $(num_constraints) = 125 ]]

C_smt_benes_thresh=1 C_pequin_io=True stack run -- c compute ./test/Code/C/pequin/ptrchase_8_8.c --setup
C_smt_benes_thresh=1 C_pequin_io=True stack run -- c compute ./test/Code/C/pequin/ptrchase_8_8.c --prove -i ./test/Code/C/pequin/inputs/ptrchase_8_8.i
stack run -- verify

C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- c flex test/Code/C/benes_arrays.c --setup
C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- c flex test/Code/C/benes_arrays.c --prove -i <(echo x 1; echo y 1)
stack run -- verify

C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- c flex test/Code/C/benes_arrays.c --setup
C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- c flex test/Code/C/benes_arrays.c --prove -i <(echo x 0; echo y 0)
stack run -- verify

C_smt_benes_thresh=1 C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- c flex test/Code/C/benes_arrays.c --setup
C_smt_benes_thresh=1 C_smt_opts=cfee,ee,mem C_smt_check_opts=True stack run -- c flex test/Code/C/benes_arrays.c --prove -i <(echo x 1; echo y 1)
stack run -- verify

stack run -- zokrates main test/Code/Zokrates/sum.zok --setup
stack run -- zokrates main test/Code/Zokrates/sum.zok --prove -i test/Code/Zokrates/inputs/sum.i
