#!/usr/bin/env python3
from yaml import load, FullLoader
from pprint import pprint
from glob import glob
import subprocess as sub
from shutil import which
from re import search
from os.path import split, join
import os

def main():
  name = "compiler-exe"
  os.environ["C_loop_bound"]="10"
  os.environ["C_c_sv"]="True"
  cc = which(name)
  assert cc is not None, f"Missing {name}"
  ps = []
  ps += glob("/home/aozdemir/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/*.yml")
  ps += glob("/home/aozdemir/repos/llcl/sv-benchmarks/c/bitvector-loops/*.yml")
  for p in ps:
    s = open(p).read()
    y = load(s, Loader = FullLoader)
    print(f"Testing: {y['input_files']}")
    bug = False

    no_overflow_props = [prop for prop in y['properties'] if 'no-overflow.prp' in prop['property_file']]
    if len(no_overflow_props) == 1:
      bug = bug or not no_overflow_props[0]['expected_verdict']

    unreach_props = [prop for prop in y['properties'] if 'unreach-call.prp' in prop['property_file']]
    if len(unreach_props) == 1:
      bug = bug or not unreach_props[0]['expected_verdict']

    c_path = join(split(p)[0], y["input_files"])

    r = sub.run([cc, "c-check", "main", c_path] , True, text=True, stderr=sub.STDOUT, stdout=sub.PIPE)
    if bug:
      if search("Bug!", r.stdout) is None:
        print(f"Expected bug in {p}, but got: {r.stdout}.")
    else:
      if search("No bug!", r.stdout) is None:
        print(f"Expected no bug in {p}, but got: {r.stdout}")

main()