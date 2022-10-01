# Matrix multiplication
# Instance:  NxN matrix "C"
# Witness:  Two NxN matrices "A", "B"
# Relation:  C = AB


from utils.sieve_test_constants import *
from utils.sieve_test_aux import *
import random
import sys
from pathlib import Path


def generate_mm(test_instance_text_path, test_name_str, test_modulus, sizes, insf, witf, relf):

    i = sizes
    test_name_str = test_family_name + "_" + str(i)
    random.seed(test_name_str)
    header_str = get_header_str(test_modulus)
    witf.write(header_str)
    witf.write("short_witness @begin\n")
    insf.write(header_str)
    insf.write("instance @begin\n")

     # write a random square matrix to both instance and witness
    isquared = i*i
    for j in range(isquared):
        randval = get_const_value_str(random.randrange(test_modulus))
        witf.write(randval)
        insf.write(randval)
    insf.write("@end\n")

    # also write the identity matrix to the witness
    for j in range(i):
       for k in range(i):
           v = 0
           if (j == k):
               v = 1
           witf.write(get_const_value_str(v))
    witf.write("@end\n")

    relf.write(header_str)
    relf.write("relation\n")
    relf.write(arithmetic_gate_set_str)
    relf.write("features: @for, @function;\n")
    relf.write("@begin\n")

    #define i-wise sum function
    relf.write("@function(sum, @out: 1, @in: "+str(i)+", @instance: 0 , @short_witness: 0)\n")
    relf.write(add_gate(i+1, 1, 2))
    relf.write("$" + str(i+2) + "...$" + str(2*i - 2) + " <- @for i @first 2 @last " + str(i-2) + "\n")
    relf.write("$(i+" + str(i) + ") <- @anon_call($(i+1), $(i+"+str(i-1)+"), @instance: 0, @short_witness: 0)\n")
    relf.write(add_gate(0,1,2))
    relf.write("@end\n") # anon call
    relf.write("@end\n") # for i=...
    relf.write(add_gate(0, i, 2*i-2))
    relf.write("@end\n") # function declaration

    # input instance
    relf.write("$0...$" + str(isquared - 1) + " <- @for i @first 0 @last "+ str(isquared - 1) + "\n")
    relf.write("$i <- @anon_call(@instance: 1, @short_witness: 0)\n$0 <- @instance;\n@end\n@end\n")
    # input witness
    relf.write("$" + str(isquared) + "...$" + str(2*isquared - 1) + " <- @for i @first " + str(isquared) + " @last "+ str(2*isquared - 1) + "\n")
    relf.write("$i <- @anon_call(@instance: 0, @short_witness: 1)\n$0 <- @short_witness;\n@end\n@end\n")
    # input witness matrix 2
    relf.write("$" + str(2*isquared) + "...$" + str(3*isquared - 1) + " <- @for i @first " + str(2*isquared) + " @last "+ str(3*isquared - 1) + "\n")
    relf.write("$i <- @anon_call(@instance: 0, @short_witness: 1)\n$0 <- @short_witness;\n@end\n@end\n")

    # multiply matrices
    relf.write("$" + str(3*isquared) + "...$" + str(4*isquared - 1) + " <- @for i @first 0 @last " + str(i-1) + "\n")
    relf.write("$(" + str(3*isquared) + " + (i * " + str(i) + ")) ... $("+ str(3*isquared + i - 1) + " + (i * " + str(i) + ")) <- @anon_call($" + str(isquared) + " ... $" + str(3*isquared - 1) + ", @instance: 0, @short_witness: 0)\n")
    # outer loop:  C'[i] = $0...$(i-1), A = $i ... $(isquared + i - 1), B = $(isquared + i) ... $(2*isquared + i - 1)
    relf.write("$0 ... $" + str(i-1) + " <- @for j @first 0 @last " + str(i-1) + "\n")
    relf.write("$j <- @anon_call($" + str(i) + " ... $" + str(2*isquared + i - 1) + ", @instance: 0, @short_witness: 0)\n")
    #inner loop:  C'[i][j] = $0, A = $1...$isquared, B = $isquared+1 ... $(2*isquared)
    relf.write("$" + str(2*isquared + 1) + " ... $" + str(2*isquared + i) + " <- @for k @first 0 @last "+str(i-1) + "\n")
    relf.write("$(k + " + str(2*isquared + 1) + ") <- @anon_call($(1 + ((i * " + str(i) + ") + k)), $(" + str(isquared+1) + " + ((k * " + str(i) + ") + j)), @instance: 0, @short_witness: 0)\n")
    relf.write(mul_gate(0,1,2))
    relf.write("@end\n") # anon call - k loop interior
    relf.write("@end\n") # for k=...
    relf.write("$0 <- @call(sum, $" + str(2*isquared + 1) + " ... $" + str(2*isquared + i) + ");\n")
    relf.write("@end\n") # anon call - j loop interior
    relf.write("@end\n") # for j=...
    relf.write("@end\n") # anon call - i loop interior
    relf.write("@end\n") # for i=...

    # check matrix equality
    relf.write("@for i @first 0 @last " + str(isquared - 1) + "\n")
    relf.write("@anon_call($i, $(i + " + str(3*isquared) + "), @instance: 0, @short_witness: 0)\n")
    relf.write(mulc_gate(2, 0, test_modulus-1))
    relf.write(add_gate(3, 1, 2))
    relf.write(assert_zero(3))
    relf.write("@end\n") # anon call 
    relf.write("@end\n") # for i=...

    relf.write("@end\n") # end relation

if __name__ == "__main__":

    if len(sys.argv) < 4 or len(sys.argv) > 5:
        print("Expecting mm.py <base_directory_name> <test_modulus> <size> optional: <max_range>")
        print("  <base_directory_name> is the target directory for the tests")
        print("  <test_modulus> is one of ", test_modulus_strings)
        print("  <size> is an N for 2**N")
        print("  <max_range> if provided, creates tests for size through max_range, inclusive")
        exit(1)

    # Capture the variables
    # The default was p2 1 10
    base_test_directory = sys.argv[1]

    # Set name for this test family
    test_family_name = "matrix-mult"
    gate_set_str = arithmetic_gate_set_str

    # Determine Modulus for this test family
    test_modulus = get_modulus(sys.argv[2])
    
    # Minimum and Maximum test values
    test_n_min = int(sys.argv[3])
    if len(sys.argv) < 5:
      test_n_max = test_n_min
    else:
      test_n_max = int(sys.argv[4])
    

    # Test sizes N (for NxN matrices)
    # test_sizes = (2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
    def gen_for_n(n):
        return 2**n
    test_sizes = map(gen_for_n, range(test_n_min, test_n_max+1))    

    generate_test_family(generate_mm, base_test_directory, test_family_name, test_modulus, gate_set_str, test_sizes)

