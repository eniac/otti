#!/usr/bin/env python3
import subprocess
import argparse
import enum
import os

class Size(enum.Enum):
    SMALL = 0
    FULL = 1

class Type(enum.Enum):
    LP = 0
    SDP = 1
    SGD = 2


def run_file(f, ty):
        codegen = ""
        if (ty == Type.LP):
            codegen="lpcodegen.py"
        elif (ty == Type.SDP):
            if not f.endswith(".dat-s"):
                print("ERROR: "+f+ " is not a dat-s file")
                return
            codegen="sdpcodegen.py"
        elif (ty == Type.SGD):
            codegen="sgdcodegen.py"
        else:
            print("ERROR: Type of "+f+ " not valid")
            return

        print("making c file for " ++ f)
        subprocess.run(["cd", "codegen/"])
        subprocess.run(["python3", codegen, dataset_path+f])
        subprocess.run(["mv", f+".c", "../out/cfiles/"])
        subprocess.run(["cd", "../compiler/"])

        print("compiling R1CS for " ++ f)
        subprocess.run(["C_outputs="+f+".zkif", "stack", "run", "--", "c", "main", "../out/cfiles/"+f+".c", "--emit-r1cs"])
        subprocess.run(["C_outputs="+f+".zkif", "stack", "run","--", "c", "main", "../out/cfiles/"+f+".c", "--prove", "-i", '<(echo "")'])
        subprocess.run(["mv", f+"inp.zkif", "../out/zkif/"])
        subprocess.run(["mv", f+"wit.zkif", "../out/zkif/"])
        subprocess.run(["mv", f+".zkif", "../out/zkif/"])
        subprocess.run(["cd", "../spartan-zkinterface/"])

        print("generating proof for " ++ f)
        subprocess.run(["./target/release/spzk", "verify", "--nizk","../out/zkif/"+f+".zkif", "../out/zkif/"+f+".inp.zkif", "../out/zkif/"+f+".wit.zkif",])


def run_dir(direc_path, ty):
        for f in os.listdir(direc_path):
            run_file(f, ty)

def parse_sdp(size=-1, ty, custom=None):
        if size == Size.SMALL:
                print("running SDP small Otti dataset")
                direc = os.fsencode("datasets/SDP/small/")
                run_dir(direc,Type.SDP)

        elif size == Size.FULL:
                print("running SDP full Otti dataset, WARNING: do not attempt this without a lot of RAM")
                direc = os.fsencode("datasets/SDP/full/")
                run_dir(direc,Type.SDP))

        elif custom != None:
                print("running SDP custom dataset")
                run_file(custom,Type.SDP));

        else:
                print("dataset for SDP not specified, running small Otti dataset")
                direc = os.fsencode("datasets/SDP/small/")
                run_dir(direc,Type.SDP))

if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    group = parser.add_mutually_exclusive_group()
    group.add_argument("--small", action='store_true')
    group.add_argument("--full", action='store_true')
    group.add_argument("--custom", type=str)

    parser.add_argument("--lp", action='store_true')
    parser.add_argument("--sdp", action='store_true')
    parser.add_argument("--sgd", action='store_true')
    args = parser.parse_args()

    size = -1;
    if args.small:
        size = Size.SMALL;

    if args.full:
        size = Size.FULL;

    if args.sdp:
        parse_sdp(size,args.custom)



