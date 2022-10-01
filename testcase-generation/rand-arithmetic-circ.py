# Randomly-generated arithmetic circuit
# Instance:  n_o-bit circuit output value
# Witness:  n_i-bit circuit input value
# Relation:  randomly-generated circuit

from utils.sieve_test_constants import *
from utils.sieve_test_aux import *
import random
import sys
from pathlib import Path

def generate_rand_arithmetic_circ(test_instance_text_path, test_name_str, test_modulus, sizes, insf, witf, relf):

    sz_in = sizes[0]
    sz_out = sizes[1]
    sz_gates = sizes[2]
    random.seed(test_name_str)
    header_str = get_header_str(test_modulus)
      
    # generate random input as witness
    witf.write(header_str)
    witf.write("short_witness @begin\n")
    wirevals = []
    for j in range(sz_in):
        randval = random.randrange(test_modulus)
        witf.write(get_const_value_str(randval))
        wirevals.append(randval)
    witf.write("@end\n")

    # generate random circuit as relation; keep track of evaluation
    relf.write(header_str)
    relf.write("relation\n")
    relf.write(arithmetic_gate_set_str)
    relf.write("features: @for;\n@begin\n")
    # read in inputs (witness wires)
    relf.write("$0 ... $" + str(sz_in - 1) + "<- @for i @first 0 @last " + str(sz_in - 1) + "\n")
    relf.write("$i <- @anon_call(@instance: 0, @short_witness: 1)\n$0 <- @short_witness;\n@end\n@end\n")
    # generate/evaluate random circuit
    for j in range(sz_gates):
        gate = random.choice(("add", "mul", "addc", "mulc"))
        if gate == "add":
            # choose two previous wires
            in1 = random.randrange(sz_in + j)
            in2 = random.randrange(sz_in + j)
            relf.write(add_gate(sz_in + j, in1, in2))
            wirevals.append((wirevals[in1] + wirevals[in2]) % test_modulus)
        elif gate == "mul":
            # choose two previous wires
            in1 = random.randrange(sz_in + j)
            in2 = random.randrange(sz_in + j)
            relf.write(mul_gate(sz_in + j, in1, in2))
            wirevals.append((wirevals[in1] * wirevals[in2]) % test_modulus)
        if gate == "addc":
            # choose one previous wire and a constant
            in1 = random.randrange(sz_in + j)
            in2 = random.randrange(test_modulus)
            relf.write(addc_gate(sz_in + j, in1, in2))
            wirevals.append((wirevals[in1] + in2) % test_modulus)
        if gate == "mulc":
            # choose one previous wire and a constant
            in1 = random.randrange(sz_in + j)
            in2 = random.randrange(test_modulus)
            relf.write(mulc_gate(sz_in + j, in1, in2))
            wirevals.append((wirevals[in1] * in2) % test_modulus)
    # Within the relation, compare the circuit outputs to the instance values.
    relf.write("$" + str(sz_in + sz_gates) + " ... $" + str(sz_in + sz_gates + sz_out - 1) + "<- @for i @first 0 @last " + str(sz_out - 1) + "\n")
    relf.write("$(i + " + str(sz_in + sz_gates) + ") <- @anon_call(@instance: 1, @short_witness: 0)\n$0 <- @instance;\n@end\n@end\n")
    relf.write("@for i @first 0 @last " + str(sz_out - 1) + "\n")
    relf.write("@anon_call($(i + " + str(sz_in + sz_gates - sz_out) + "), $(i + " + str(sz_in + sz_gates) + "), @instance: 0, @short_witness: 0)\n")
    relf.write(mulc_gate(2, 0, test_modulus - 1))
    relf.write(add_gate(3, 1, 2))
    relf.write(assert_zero(3))
    relf.write("@end\n") # anon call
    relf.write("@end\n") # for i=...
    relf.write("@end\n") # relation

    insf.write(header_str)
    insf.write("instance @begin\n")

    # write the relevant output values to the instance file
    for j in range(sz_out):
        insf.write(get_const_value_str(wirevals[sz_in + sz_gates - sz_out + j]))
    insf.write("@end\n")
   
if __name__ == "__main__":

    if False and len(sys.argv) < 4 or len(sys.argv) > 5:
        print("Expecting rand-arithmetic-circ.py <base_directory_name> <test_modulus> <size> optional: <max_range>")
        print("  <base_directory_name> is the target directory for the tests")
        print("  <test_modulus> is one of ", test_modulus_strings)
        print("  <size> N for random circuit with 10N input wires, 10N output wires, and 100N**2 gates")
        print("  <max_range> if provided, creates tests for size through max_range, inclusive")
        exit(1)

    # Capture the variables
    # The default was p3 6 10
    base_test_directory = sys.argv[1]
    
    # Set name for this test family
    test_family_name = "rand-arithmetic-circ"
    gate_set_str = arithmetic_gate_set_str
    
    
    # Determine Modulus for this test family
    test_modulus = get_modulus(sys.argv[2])
    
    # Minimum and Maximum test values
    test_n_min = int(sys.argv[3])
    if len(sys.argv) < 5:
      test_n_max = test_n_min
    else:
      test_n_max = int(sys.argv[4])

    # Test sizes (n_i, n_o, n_g):  creates a random circuit with n_i input wires, n_o output wires, and n_g gates
    #test_sizes = ((60, 60, 6400), (70, 70, 12800), (80, 80, 25600), (90, 90, 51200), (100, 100, 102400))
    def gen_for_n(n):
        return (10*n, 10*n, 100*2**n)
    test_sizes = map(gen_for_n, range(test_n_min, test_n_max+1))

    generate_test_family(generate_rand_arithmetic_circ, base_test_directory, test_family_name, test_modulus, gate_set_str, test_sizes)
