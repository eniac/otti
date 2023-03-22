# 3SAT
# Instance:  <nothing>
# Witness:  truth assignment (1/0) to n variables
# Relation:  verification of a randomly-generated 3SAT instance with n variables, m clauses

from utils.sieve_test_constants import *
from utils.sieve_test_aux import *
import random
import sys
from pathlib import Path
import os

def generate_sgd(test_instance_fb_path, test_name_str, test_modulus, sizes, insf, witf, relf):

    num_samples = str(sizes[0])
    num_features = str(sizes[1])
    
    home = str(os.getcwd())

    name = str(num_features)+"_feat_"+str(num_samples)+"_samples_"+str(test_modulus)[:10]

    print("Generate inputs for " + name)
    cfile = "sgd_synthetic_"+name+".c"
    wfile = "sgd_synthetic_"+name+".in"

    subprocess.run(["python3", home+"/codegen/syntheticsgd.py", cfile,\
        wfile, num_samples, num_features])
    subprocess.run(["mv", cfile, home+"/out/cfiles/"])
    subprocess.run(["mv", wfile, home+"/out/wit/"])

    print("Compile, solve, and prove " + name)
    os.chdir(home+"/circ/")
    subprocess.run("./target/release/examples/circ --inputs "\
        +home+"/out/wit/"+wfile+" "+home+"/out/cfiles/"+cfile+" r1cs --action sieve"\
        +" --custom-mod "+str(test_modulus)+" --outdir "+home+"/out/sieve/"+name, shell=True)

    os.chdir(home)
    # TODO MV
    subprocess.run(["cp", home+"/out/sieve/"+name+"/000_public_inputs_0.sieve",\
                    home+"/"+str(test_instance_fb_path)+"/instance/"])
    subprocess.run(["cp", home+"/out/sieve/"+name+"/001_private_inputs_0.sieve",\
                    home+"/"+str(test_instance_fb_path)+"/witness/"])
    subprocess.run(["cp", home+"/out/sieve/"+name+"/002_relation.sieve",\
                    home+"/"+str(test_instance_fb_path)+"/relation/"])


if __name__ == "__main__":

    if len(sys.argv) < 4 or len(sys.argv) > 5:
        print("Expecting sgd.py <base_directory_name> <test_modulus> <size> optional: <max_range>")
        print("  <base_directory_name> is the target directory for the tests")
        print("  <test_modulus> is one of ", test_modulus_strings)
        print("  <size> is an N for sgd instance with N samples, N features")
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

    home = str(os.getcwd())
    if not os.path.isdir(home+"/out"):
        subprocess.run(["mkdir", home+"/out"])
    if not os.path.isdir(home+"/out/cfiles"):
        subprocess.run(["mkdir", home+"/out/cfiles"])
    if not os.path.isdir(home+"/out/wit"):
        subprocess.run(["mkdir", home+"/out/wit"])
    if not os.path.isdir(home+"/out/sieve"):
        subprocess.run(["mkdir", home+"/out/sieve"])
    
    # Creates SGD instance with n variables, m clauses
    def gen_for_n(n):
        return (n, 10*n)
    test_sizes = map(gen_for_n, range(test_n_min, test_n_max+1, 1))

    generate_test_family(generate_sgd, base_test_directory, test_family_name, test_modulus, gate_set_str, test_sizes)
