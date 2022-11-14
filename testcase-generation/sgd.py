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


def generate_sgd(test_instance_text_path, test_name_str, test_modulus, sizes, insf, witf, relf):

    num_samples = sizes[0]
    num_features = sizes[1]
    test_name_str = test_family_name + "_" + str(sz_vars) + "_" + str(sz_clauses)
    random.seed(test_name_str)

    # Create a random synthetic instance first
    subprocess.run(["python3", home+"/codegen/syntheticsgd.py", insf, witf, \
        num_samples num_features])

    # run CirC to compile and generate seive ir
    os.chdir(home+"/rust-circ/")
    subprocess.run("./target/release/examples/circ --inputs \
            "+home+"/out/wit/"+wfile+" "+home+"/out/cfiles/"+cfile+" r1cs \
            --action seive --custom_mod"+test_modulus, shell=True)

    # naming ?

if __name__ == "__main__":

    if len(sys.argv) < 4 or len(sys.argv) > 5:
        print("Expecting sgd.py <base_directory_name> <test_modulus> <size> optional: <max_range>")
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
      
    
    # Creates SGD instance with n variables, m clauses
    def gen_for_n(n):
        first = n
        return (first, first)
    test_sizes = map(gen_for_n, range(test_n_min, test_n_max+1))

    generate_test_family(generate_sgd, base_test_directory, test_family_name, test_modulus, gate_set_str, test_sizes)
