#!/usr/bin/env python3
from yaml import load, FullLoader
from pprint import pprint
from glob import glob
import subprocess as sub
from shutil import which
from re import search
from os.path import split, join

def main():
  name = "compiler-exe"
  cc = which(name)
  assert cc is not None, f"Missing {name}"
  ps = glob("/home/aozdemir/repos/llcl/sv-benchmarks/c/signedintegeroverflow-regression/*.yml")
  for p in ps:
    s = open(p).read()
    y = load(s, Loader = FullLoader)
    no_overflow_props = [prop for prop in y['properties'] if 'no-overflow.prp' in prop['property_file']]
    assert len(no_overflow_props) == 1, f"Should be a prop for no overflow in {p}"
    overflow = not no_overflow_props[0]['expected_verdict']
    c_path = join(split(p)[0], y["input_files"])
    r = sub.run([cc, "c-check", "main", c_path] , True, text=True, stderr=sub.STDOUT, stdout=sub.PIPE)
    if overflow:
      if search("Bug!", r.stdout) is None:
        print(f"Expected overflow in {p}, but got: {r.stdout}.")
    else:
      if search("No bug!", r.stdout) is None:
        print(f"Expected no overflow in {p}, but got: {r.stdout}")

main()