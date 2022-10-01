# 3SAT
# Instance:  <nothing>
# Witness:  truth assignment (1/0) to n variables
# Relation:  verification of a randomly-generated 3SAT instance with n variables, m clauses

from utils.sieve_test_constants import *
from utils.sieve_test_aux import *
import random
import sys
from pathlib import Path
from bitstring import BitArray


def generate_threesat(test_instance_text_path, test_name_str, test_modulus, sizes, insf, witf, relf):

    sz_vars = sizes[0]
    sz_clauses = sizes[1]
    test_name_str = test_family_name + "_" + str(sz_vars) + "_" + str(sz_clauses)
    random.seed(test_name_str)

    # Create a random witness first
    witness_bits = BitArray(uint=random.getrandbits(sz_vars), length=sz_vars)

    # Write witness file
    header_str = get_header_str(test_modulus)
    witf.write(header_str)
    witf.write("short_witness @begin\n")
    for i in range(sz_vars):
        witf.write(get_const_value_str(witness_bits[i]))
    witf.write("@end\n")

    # Write (trivial) instance file
    insf.write(header_str)
    insf.write("instance @begin\n@end\n")

    # Write relation file
    relf.write(header_str)
    relf.write("relation\n")
    relf.write(boolean_gate_set_str)
    relf.write("features: simple;\n@begin\n")

    # Witness wires are inputs 0 through sz_vars-1
    for i in range(sz_vars):
        relf.write(input_witness(i))

    # Generate clauses.  The i-th clause will use wires (sz_vars + 5i) through (sz_vars + 5i + 4).
    num_clauses_generated = 0
    current_wire = sz_vars
    while (num_clauses_generated < sz_clauses):
        # generate a random clause
        index1 = random.randrange(sz_vars)
        index2 = random.randrange(sz_vars)
        index3 = random.randrange(sz_vars)
        neg1 = bool(random.randint(0,1))
        neg2 = bool(random.randint(0,1))
        neg3 = bool(random.randint(0,1))
        # check if our witness satisfies the randomly-generated clause
        if ((witness_bits[index1] != neg1) or (witness_bits[index2] != neg2) or (witness_bits[index3] != neg3)):
            # Add this clause to our relation.  
            # The relation performs zero-checks, so it checks that not(a or b or c) == 0 , where a, b, and c are the possibly-negated terms.
            # The left-hand side is equivalent to (not(a) and not(b) and not(c)).  
            # If any of a, b, or c are a negation, then the not() simply becomes a copy gate.
            if (neg1):
                relf.write(copy_gate(current_wire, index1))
            else:
                relf.write(not_gate(current_wire, index1))
            if (neg2):
                relf.write(copy_gate(current_wire + 1, index2))
            else:
                relf.write(not_gate(current_wire + 1, index2))
            if (neg3):
                relf.write(copy_gate(current_wire + 2, index3))
            else:
                relf.write(not_gate(current_wire + 2, index3))
            relf.write(and_gate(current_wire + 3, current_wire, current_wire + 1))  # (not(a) and not(b))
            relf.write(and_gate(current_wire + 4, current_wire + 3, current_wire + 2)) # (not(a) and not(b) and not(c))
            relf.write(assert_zero(current_wire + 4))
            relf.write(delete_range(current_wire, current_wire + 4))
            num_clauses_generated = num_clauses_generated + 1
            current_wire = current_wire + 5
    relf.write("@end\n")

if __name__ == "__main__":

    if len(sys.argv) < 4 or len(sys.argv) > 5:
        print("Expecting threesat.py <base_directory_name> <test_modulus> <size> optional: <max_range>")
        print("  <base_directory_name> is the target directory for the tests")
        print("  <test_modulus> is one of ", test_modulus_strings)
        print("  <size> is an N for 3SAT instance with 2**N variables, 2*2**N clauses")
        print("  <max_range> if provided, creates tests for size through max_range, inclusive")
        exit(1)


    # Capture the variables
    # The default was p0 3 12
    base_test_directory = sys.argv[1]

    # Set name for this test family
    test_family_name = "sgd"
    gate_set_str = boolean_gate_set_str


    
    # Determine Modulus for this test family
    test_modulus = get_modulus(sys.argv[2])
    
    # Minimum and Maximum test values
    test_n_min = int(sys.argv[3])
    if len(sys.argv) < 5:
      test_n_max = test_n_min
    else:
      test_n_max = int(sys.argv[4])
      
    
    # Creates 3SAT instance with n variables, m clauses
    # test_sizes = ((256, 512), (512, 1024))
    def gen_for_n(n):
        first = 2**n
        return (first, first*2)
    test_sizes = map(gen_for_n, range(test_n_min, test_n_max+1))

    generate_test_family(generate_threesat, base_test_directory, test_family_name, test_modulus, gate_set_str, test_sizes)
