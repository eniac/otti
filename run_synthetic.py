#!/usr/bin/env python3
import subprocess
import argparse
import enum
import os
import json

from codegen import lpcodegen

class Size(enum.Enum):
    SMALL = 0
    FULL = 1

class Type(enum.Enum):
    LP = 0
    SDP = 1
    SGD = 2

def absoluteFilePaths(directory):
    for dirpath,_,filenames in os.walk(directory):
        for f in filenames:
            yield os.path.abspath(os.path.join(dirpath, f)).decode("utf-8")

def run_sgd(home, name,field, n, m):
   
    print("Generate inputs for " + name)
    cfile = "sgd_synthetic_"+name+".c"
    wfile = "sgd_synthetic_"+name+".in"

    subprocess.run(["python3", home+"/codegen/syntheticsgd.py", cfile,\
        wfile, n, m])
    subprocess.run(["mv", cfile, home+"/out/cfiles/"])
    subprocess.run(["mv", wfile, home+"/out/wit/"])

    print("Compile, solve, and prove " + name)
    os.chdir(home+"/rust-circ/")
    subprocess.run("./target/release/examples/circ --inputs\
            "+home+"/out/wit/"+wfile+" "+home+"/out/cfiles/"+cfile+" r1cs\
            --action zkif", shell=True)

    # TODO feild, zkif

if __name__ == "__main__":
    home = os.getcwd()

    parser = argparse.ArgumentParser()

    #group = parser.add_mutually_exclusive_group()
    #group.add_argument("--lp", action='store_true')
    #parser.add_argument("--sdp", action='store_true')
    #parser.add_argument("--sgd", action='store_true')
    
    ristretto255 = 7237005577332262213973186563042994240857116359379907606001950938285454250989

    parser.add_argument("--n", type=int, default=10)
    parser.add_argument("--m", type=int, default=10)
    parser.add_argument("--field", type=int, default=ristretto255)
    parser.add_argument("--name", type=str, required=True)

    args = parser.parse_args()

    if not os.path.isdir(home+"/out"):
        subprocess.run(["mkdir", home+"/out"])
    if not os.path.isdir(home+"/out/cfiles"):
        subprocess.run(["mkdir", home+"/out/cfiles"])
    if not os.path.isdir(home+"/out/zkif"):
        subprocess.run(["mkdir", home+"/out/zkif"])
    if not os.path.isdir(home+"/out/wit"):
        subprocess.run(["mkdir", home+"/out/wit"])


    run_sgd(home,args.name,str(args.field),str(args.n),str(args.m))

    '''
    if args.lp:
        inputs = parse_lp(home,size,args.custom)
        run_lp(home, inputs)

    elif args.sdp:
        parse_sdp(home,size,args.custom)
        subprocess.run(["rm", home+"/compiler/C",  home+"/compiler/x", home+"/compiler/w"])


    if args.sgd:
        parse_sgd(home,size,args.custom)

    else:
        parser.print_help()

    '''
