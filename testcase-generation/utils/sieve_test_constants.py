# Handy constants and functions for SIEVE test case generation.  
import sys

# SIEVE test moduli
# (prime, "p#_desc")
#test_modulus_p0 = (2, "p0_bool") # Boolean
#test_modulus_p1 = (2305843009213693951, "p1_2to61-1") # 2^61 - 1
#test_modulus_p2 = (21888242871839275222246405745257275088548364400416034343698204186575808495617, "p2_bn128")   # order of BN128 curve
#test_modulus_p3 = (1073479681, "p3_short")
#test_modulus_p4 = (57896044618658097711785492504343953926634992332820282019728792003956564819949, "p4_2to255-19")  # 2^255 - 19
test_modulus_p0 = (2, "p0") # Boolean
test_modulus_p1 = (2305843009213693951, "p1") # 2^61 - 1
test_modulus_p2 = (21888242871839275222246405745257275088548364400416034343698204186575808495617, "p2")   # order of BN128 curve
test_modulus_p3 = (1073479681, "p3")
test_modulus_p4 = (57896044618658097711785492504343953926634992332820282019728792003956564819949, "p4")  # 2^255 - 19

# For interface usage
test_modulus_strings = "p0, p1, p2, p3, p4"
modulus_map = dict()
modulus_map["p0"] = test_modulus_p0
modulus_map["p1"] = test_modulus_p1
modulus_map["p2"] = test_modulus_p2
modulus_map["p3"] = test_modulus_p3
modulus_map["p4"] = test_modulus_p4
def get_modulus(s):
  if s in modulus_map:
    return modulus_map[s]
  else:
    print("ERROR: Modulus must be in", test_modulus_strings)
    sys.exit(["ERROR: Modulus must be in " + test_modulus_strings])



# Header information
def get_header_str(test_modulus):
    return "version 1.0.0;\nfield characteristic " + str(test_modulus) + " degree 1;\n"
arithmetic_gate_set_str = "gate_set:  arithmetic;\n"
boolean_gate_set_str = "gate_set:  boolean;\n"

# Handy functions for creating directives in SIEVE IR1
# Boolean
def and_gate(output, input1, input2):
    return "$" + str(output) + " <- @and($" + str(input1) + ", $" + str(input2) + ");\n"
def xor_gate(output, input1, input2):
    return "$" + str(output) + " <- @xor($" + str(input1) + ", $" + str(input2) + ");\n"
def not_gate(output, input):
    return "$" + str(output) + " <- @not($" + str(input) + ");\n"
# Arithmetic
def mul_gate(output, input1, input2):
    return "$" + str(output) + " <- @mul($" + str(input1) + ", $" + str(input2) + ");\n"
def add_gate(output, input1, input2):
    return "$" + str(output) + " <- @add($" + str(input1) + ", $" + str(input2) + ");\n"
def mulc_gate(output, input1, input2):
    return "$" + str(output) + " <- @mulc($" + str(input1) + ", <" + str(input2) + ">);\n"
def addc_gate(output, input1, input2):
    return "$" + str(output) + " <- @addc($" + str(input1) + ", <" + str(input2) + ">);\n"
# Common
def copy_gate(output, input):
    return "$" + str(output) + " <- $" + str(input) + ";\n"
def constant_gate(output, input):
    return "$" + str(output) + " <- <" + str(int(input)) + ">;\n"  # the int() conversion is so this works with booleans
def assert_zero(var):
    return "@assert_zero($" + str(var) + ");\n"
def input_witness(var):
    return "$" + str(var) + " <- @short_witness;\n"
def input_instance(var):
    return "$" + str(var) + " <- @instance;\n"
def delete(var):
    return "@delete($" + str(var) + ");\n"
def delete_range(begin, end):
    return "@delete($" + str(begin) + ", $" + str(end) + ");\n"

def get_const_value_str(var):
    return "<" + str(int(var)) + ">;\n"
