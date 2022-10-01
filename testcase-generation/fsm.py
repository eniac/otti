# Verify end state of a finite state machine after N steps
# instance:  (claimed) output state
# witness:  N inputs (0...E-1)
# relation:  (randomly-generated) state machine with S states, E edges per state

from utils.sieve_test_constants import *
from utils.sieve_test_aux import *
import random
import sys
from pathlib import Path

def generate_fsm(test_instance_text_path, test_name_str, test_modulus, sizes, insf, witf, relf):

    nstates = sizes[0]
    nedges = sizes[1]
    nsteps = sizes[2]
    test_name_str = test_family_name + "_" + str(nstates) + "_" + str(nedges) + "_" + str(nsteps)
    random.seed(test_name_str)
    
    header_str = get_header_str(test_modulus)

    # Create a list-of-lists defining the FSM transitions.  When in state a and given input b, transition to state fsm[a][b].
    # Populate the transitions randomly.
    fsm = []
    for j in range(nstates):
        state = []
        for k in range(nedges):
            randval = random.randrange(nstates)
            state.append(randval)
        fsm.append(state)

    witf.write(header_str)
    witf.write("short_witness @begin\n")
    # witness is a random sequence of inputs.  Keep track of the FSM state as we go
    curr_state = 0
    for j in range(nsteps):
        randinput = random.randrange(nedges)
        curr_state = fsm[curr_state][randinput]
        witf.write(get_const_value_str(randinput))
    witf.write("@end\n")

    insf.write(header_str)
    insf.write("instance @begin\n")
    insf.write(get_const_value_str(curr_state)) # write end state to witness
    insf.write("@end\n")

    relf.write(header_str)
    relf.write("relation\n")
    relf.write(arithmetic_gate_set_str)
    relf.write("features: @for, @switch;\n")
    relf.write("@begin\n")

    # loop over steps of state transitions.  
    # the value on wire $i is the state of the FSM after i steps.
    relf.write(constant_gate(0, 0))
    relf.write("$1...$" + str(nsteps) + " <- @for i @first 1 @last " + str(nsteps) + "\n")
    relf.write("$i <- @anon_call($(i - 1), @instance: 0, @short_witness: 1)\n")
    relf.write(input_witness(2))

    # write FSM transitions as a 2-nested switch/case
    # switch over current state
    relf.write("$0 <- @switch($1)\n")
    for j in range(nstates):
        relf.write("@case <" + str(j) + ">:\n")
        relf.write("@anon_call($2, @instance: 0, @short_witness: 0)\n")

        # switch over outgoing transitions of this state
        relf.write("$0 <- @switch($1)\n")
        for k in range(nedges):
            relf.write("@case <" + str(k) + ">:\n")
            relf.write("@anon_call(@instance: 0, @short_witness: 0)\n" + constant_gate(0, fsm[j][k]) + "@end\n")
        relf.write("@end\n") # switch(transitions)

        relf.write("@end\n") # anon_call for case j
    relf.write("@end\n") # switch(states)

    relf.write("@end\n") # anon_call - body of loop over transition steps
    relf.write("@end\n") # for loop over transition steps

    # at this point the final state after N steps should be in wire $(nsteps).
    # read in claimed state from instance and compare.
    relf.write(input_instance(nsteps + 1))
    relf.write(mulc_gate(nsteps + 2, nsteps + 1, test_modulus - 1))
    relf.write(add_gate(nsteps + 3, nsteps + 2, nsteps))
    relf.write(assert_zero(nsteps + 3))

    relf.write("@end\n") # relation


if __name__ == "__main__":

    if len(sys.argv) < 4 or len(sys.argv) > 5:
        print("Must provide the base directory where test families are stored")
        print("Expecting fsm.py <base_directory_name> <test_modulus> <size> optional: <max_range>")
        print("  <base_directory_name> is the target directory for the tests")
        print("  <test_modulus> is one of ", test_modulus_strings)
        print("  <size> is an N for (10N states, 2N edges per state, 2**N steps)")
        print("  <max_range> if provided, creates tests for size through max_range, inclusive")
        exit(1)

    # Capture the variables
    # The default was p2 7 20
    base_test_directory = sys.argv[1]
    
    # Set name for this test family
    test_family_name = "fsm"
    gate_set_str = arithmetic_gate_set_str
    
    # Determine Modulus for this test family
    test_modulus = get_modulus(sys.argv[2])
    
    # Minimum and Maximum test values
    test_n_min = int(sys.argv[3])
    if len(sys.argv) < 5:
      test_n_max = test_n_min
    else:
      test_n_max = int(sys.argv[4])

    # Test sizes:  (S, E, N) = (# states, # edges per state, # steps)
    def gen_for_n(n):
      return (10*n, 2*n, 2**n)
    test_sizes = map(gen_for_n, range(test_n_min, test_n_max+1))
    #test_sizes = ((70,14,128),(80,16,256),(90,18,512),(100,20,1024),(110,22,2048),(120,24,4096),(130,26,8192),(140,28,16384),(150,30,32768),(160,32,65536),(170,34,131072),(180,36,262144),(190,38,524288),(200,40,1048576))
    #def gen_for_n(n):
    #    #note: intended was 10N, 2N, 100*2^n originally
    #    return (10*n, 2*n, 2**n)
    #test_sizes = map(gen_for_n, range(1, 7))

    generate_test_family(generate_fsm, base_test_directory, test_family_name, test_modulus, gate_set_str, test_sizes)
