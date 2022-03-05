#!/usr/bin/env python3
import subprocess
import argparse

print("Placeholder")

SMALL = 0
FULL = 1


def run_sdp(dataset_path):
        for f in dataset_path:
                if f.endswith(".dat-s"):

                        print("making c file for " ++ f)
                        subprocess.run(["cd", "codegen/"])
                        subprocess.run(["python3", "sdpcodegen.py", dataset_path+f])
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
                        subprocess.run(["", "", ""])
                else:
                        print(f++" is not a .dat-s file")


def parse_sdp(size=-1, custom=None):
        if size == SMALL:
                print("running SDP small Otti dataset")
                run_sdp("datasets/SDP/small/");

        elif size == FULL:
                print("running SDP full Otti dataset, WARNING: do not attempt this without a lot of RAM")
                run_sdp("datasets/SDP/full/");

        elif custom != None:
                print("running SDP custom dataset")
#ends with /?
                run_sdp("");

        else:
                print("dataset for SDP not specified, running small Otti dataset")
                run_sdp("datasets/SDP/small/");


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
        size = SMALL;

    if args.full:
        size = FULL;

    if args.sdp:
        parse_sdp(size,args.custom)



