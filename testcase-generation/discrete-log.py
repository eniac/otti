from utils.sieve_test_constants import *
from utils.sieve_test_aux import *
import random
import math
import sys
from pathlib import Path

# Verify N discrete logs in the field
# instance:  N random values
# witness:  N discrete logs (expressed in binary for convenience)


# The sizes value should contain an int value
def generate_discrete_log(test_instance_text_path, test_name_str, test_modulus, size, insf, witf, relf):

    # Select base for discrete log
    base = 2
    num_bits = 1 + math.floor(math.log2(test_modulus))

    test_name_str = test_family_name + "_" + str(size)
    random.seed(test_name_str)
    header_str = get_header_str(test_modulus)
    witf.write(header_str)
    witf.write("short_witness @begin\n")
    insf.write(header_str)
    insf.write("instance @begin\n")
    insf.write(get_const_value_str(base))

    for j in range(size):
        randexp = random.randrange(test_modulus)
        pwr = pow(base, randexp, test_modulus)
        insf.write(get_const_value_str(pwr))
        #break witness into bits, write them high-order-bits first
        randexpbits = []
        for k in range(num_bits):
            randexpbits.append(randexp % 2)
            randexp = randexp // 2
        for k in range(num_bits):
            witf.write(get_const_value_str(randexpbits[num_bits - k - 1]))
    insf.write("@end\n")
    witf.write("@end\n")

    relf.write(header_str)
    relf.write("relation\n")
    relf.write(arithmetic_gate_set_str)
    relf.write("features: @for, @switch;\n")
    relf.write("@begin\n")

    # read base from instance
    relf.write(input_instance(0))

    relf.write("@for i @first 0 @last " + str(size-1) + "\n")
    relf.write("@anon_call($0, @instance: 1, @short_witness: " + str(num_bits) + ")\n")

    # read high-order bit of exponent
    relf.write(input_witness(1))
    relf.write("$2 <- @switch($1)\n")
    relf.write("@case <1> : @anon_call(@instance: 0, @short_witness: 0)\n" + constant_gate(0, base) + "@end\n")
    relf.write("@case <0> : @anon_call(@instance: 0, @short_witness: 0)\n" + constant_gate(0, 1) + "@end\n")
    relf.write("@end\n")  # switch

    # repeated squaring
    relf.write("$3...$" + str(num_bits + 1) + " <- @for j @first 1 @last " + str(num_bits-1) + "\n")
    relf.write("$(j+2) <- @anon_call($(j+1), @instance: 0, @short_witness: 1)\n")
    # square previous value
    relf.write(mul_gate(2,1,1))
    # either multiply by base, or don't, depending on the next bit of the witness
    relf.write(input_witness(3))
    relf.write("$0 <- @switch($3)\n")
    relf.write("@case <1> : @anon_call($2, @instance: 0, @short_witness: 0)\n" + mulc_gate(0,1,base) + "@end\n")
    relf.write("@case <0> : @anon_call($2, @instance: 0, @short_witness: 0)\n" + copy_gate(0,1) + "@end\n")
    relf.write("@end\n") # switch
    relf.write("@end\n") # anon_call(j...)
    relf.write("@end\n") # for j=...

    # At this point the $(num_bits+1) wire should contain base^witness
    relf.write(input_instance(num_bits + 2))
    relf.write(mulc_gate(num_bits + 3, num_bits + 2, test_modulus - 1))
    relf.write(add_gate(num_bits + 4, num_bits + 3, num_bits + 1))
    relf.write(assert_zero(num_bits + 4))

    relf.write("@end\n") # anon_call
    relf.write("@end\n") # for i=...
    relf.write("@end\n") # relation


if __name__ == "__main__":

    if len(sys.argv) < 4 or len(sys.argv) > 5:
        print("Expecting discrete-log.py <base_directory_name> <test_modulus> <size> optional: <max_range>")
        print("  <base_directory_name> is the target directory for the tests")
        print("  <test_modulus> is one of ", test_modulus_strings)
        print("  <size> is an N for log base of 2**N")
        print("  <max_range> if provided, creates tests for size through max_range, inclusive")
        exit(1)

    # Capture the variables
    # The default was p1 8 16
    base_test_directory = sys.argv[1]

    # Set name for this test family
    test_family_name = "discrete-log"
    gate_set_str = arithmetic_gate_set_str
    
    # Determine Modulus for this test family
    test_modulus = get_modulus(sys.argv[2])
    
    # Minimum and Maximum test values
    test_n_min = int(sys.argv[3])
    if len(sys.argv) < 5:
      test_n_max = test_n_min
    else:
      test_n_max = int(sys.argv[4])
    
    # Creates a discrete log, base 2, with size N
    # test_sizes example = (512, 1024, 2048, 4096)
    def gen_for_n(n):
        return 2**n
    test_sizes = map(gen_for_n, range(test_n_min, test_n_max+1))
    

    generate_test_family(generate_discrete_log, base_test_directory, test_family_name, test_modulus, gate_set_str, test_sizes)
