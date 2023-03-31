# Utility functions for structured SIEVE test case generation.  

import subprocess
import sys
from pathlib import Path

# Called directly to create the test family
# Note that the generation function itself is passed
def generate_test_family(generation_fuction, base_test_directory, test_family_name, test_modulus, gate_set_str, test_sizes):
    # produce the test family directory if necessary
    test_family_path = create_test_family_directory(base_test_directory, test_family_name, test_modulus)

    # Iterate through each size instance to create the structure
    for sizes in test_sizes:
        print("Running size " + str(sizes))
        if type(sizes) == int:
            str_sizes = str(sizes)
        else:
            str_sizes = '_'.join(map(str,sizes))
        test_name_str = test_family_name + "_" + str_sizes
        text_path, flatbuffer_path = create_test_instance_directory(test_family_path, test_name_str)


        instance_path = text_path / "instance/instance.txt"
        witness_path = text_path / "witness/witness.txt"
        relation_path = text_path / "relation/relation.txt"
        flat_instance_path = flatbuffer_path / "instance/000_public_inputs_0.sieve"
        flat_witness_path = flatbuffer_path / "witness/001_private_inputs_0.sieve"
        flat_relation_path = flatbuffer_path / "relation/002_relation.sieve"
        if (witness_path.is_file() and relation_path.is_file() and instance_path.is_file() and flat_witness_path.is_file() and flat_relation_path.is_file() and flat_instance_path.is_file()):
            continue

        with open(instance_path, "w") as insf, open(witness_path, "w") as witf, open(relation_path, "w") as relf:
            generation_fuction(flatbuffer_path, test_name_str, test_modulus[0], sizes, insf, witf, relf)
        subprocess.run(["~/zkinterface-sieve/rust/target/release/zki_sieve","to-yaml", str(flat_instance_path)], stdout=open(str(instance_path), 'w'))
        subprocess.run(["~/zkinterface-sieve/rust/target/release/zki_sieve","to-yaml", str(flat_witness_path)],
                stdout=open(str(witness_path), 'w'))
        subprocess.run(["~/zkinterface-sieve/rust/target/release/zki_sieve","to-yaml", str(flat_relation_path)],
                stdout=open(str(relation_path), 'w'))
        

# Takes a Path object and creates subdirectories for instance, witness, and relation
def populate_subfolders(base_directory):
    if base_directory.exists() and base_directory.is_dir():
        instance_path = base_directory / "instance"
        witness_path = base_directory / "witness"
        relation_path = base_directory / "relation"
        try:
            instance_path.mkdir(exist_ok=True)
            witness_path.mkdir(exist_ok=True)
            relation_path.mkdir(exist_ok=True)
        except FileExistsError:
            print("Path to instance, witness, or relation exists and is not a directory")
            exit(1)
    else: 
        print("Base directory provided does not exist: " + str(base_directory) )
        exit(1)

# Create a directory for a test family
def create_test_family_directory(base_test_directory, test_family_name, test_modulus):
    testdir_path = Path(base_test_directory)
    if testdir_path.exists() and testdir_path.is_dir():
        test_family_path = testdir_path / (test_family_name + "_" + test_modulus[1])
        try:
            test_family_path.mkdir(exist_ok=True)
        except FileExistsError as err:
            print("Test family directory path exists and is not a directory")
            exit(1)
        else:
            return test_family_path
    else: 
        print("Test directory provided does not exist: ", testdir_path)
        exit(1)
        
# Create a directory for a test instance
def create_test_instance_directory(test_family_path, test_name_str):
    if test_family_path.exists() and test_family_path.is_dir():
        test_instance_path = test_family_path / test_name_str
        try:
            test_instance_path.mkdir(exist_ok=True)
            flatbuffer_path = test_instance_path / "flatbuffer"
            flatbuffer_path.mkdir(exist_ok=True)
            populate_subfolders(flatbuffer_path)
            text_path = test_instance_path / "text"
            text_path.mkdir(exist_ok=True)
            populate_subfolders(text_path)
            return (text_path, flatbuffer_path)
        except FileExistsError:
            print("Test instance directory path exists, is not a directory")
            exit(1)
    else: 
        print("Test directory provided does not exist: ", test_family_path)
        exit(1)




