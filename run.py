#!/usr/bin/env python3
import subprocess
import argparse
import enum
import os
import json

def run_sgd(home, n, m, field):
    name = n+"_feat_"+m+"_samples"

    print("Generate inputs for " + name)
    cfile = "sgd_synthetic_"+name+".c"
    wfile = "sgd_synthetic_"+name+".in"

    subprocess.run(["python3", home+"/codegen/syntheticsgd.py", cfile,\
        wfile, n, m])
    subprocess.run(["mv", cfile, home+"/out/cfiles/"])
    subprocess.run(["mv", wfile, home+"/out/wit/"])

    print("Compile, solve, and prove " + name)
    os.chdir(home+"/rust-circ/")
    subprocess.run("./target/release/examples/circ\
            "+home+"/out/cfiles/"+cfile+" r1cs --action sieve\
            --inputs "+home+"/out/wit/"+wfile+" --custom-mod "+field, shell=True)

if __name__ == "__main__":
    home = os.getcwd()

    parser = argparse.ArgumentParser()

    ristretto255 = 7237005577332262213973186563042994240857116359379907606001950938285454250989

    parser.add_argument("--n", type=int, default=10)
    parser.add_argument("--m", type=int, default=10)
    parser.add_argument("--field", type=int, default=ristretto255)

    args = parser.parse_args()

    if not os.path.isdir(home+"/out"):
        subprocess.run(["mkdir", home+"/out"])
    if not os.path.isdir(home+"/out/cfiles"):
        subprocess.run(["mkdir", home+"/out/cfiles"])
    if not os.path.isdir(home+"/out/zkif"):
        subprocess.run(["mkdir", home+"/out/zkif"])
    if not os.path.isdir(home+"/out/wit"):
        subprocess.run(["mkdir", home+"/out/wit"])


    run_sgd(home,str(args.n),str(args.m),str(args.field))

