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


def run_file(name, path_name, ty, home):
        f = name.decode('UTF-8')
        path = path_name.decode('UTF-8')
        codegen = ""
        if (ty == Type.LP):
            codegen=home+"/codegen/lpcodegen.py"
        elif (ty == Type.SDP):
            if not f.endswith('.dat-s'):
                print("ERROR: "+f+ " is not a dat-s file")
                return
            codegen=home+"/codegen/sdpcodegen.py"
        else:
            print("ERROR: Type of "+f+ " not valid")
            return

        print("Making c file for " + f)
        subprocess.run(["python3", codegen, path+f])
        subprocess.run(["mv", f+".c", home+"/out/cfiles/"])
        os.chdir(home+"/compiler/")
        
        print("Compiling R1CS for " + f)
        subprocess.run("C_outputs="+f+".zkif stack run -- c main "+home+"/out/cfiles/"+f+".c --emit-r1cs", shell=True)
        subprocess.run("C_outputs="+f+".zkif stack run -- c main "+home+"/out/cfiles/"+f+".c --prove -i input", shell=True)
        subprocess.run(["mv", f+".inp.zkif", home+"/out/zkif/"])
        subprocess.run(["mv", f+".wit.zkif", home+"/out/zkif/"])
        subprocess.run(["mv", f+".zkif", home+"/out/zkif/"])
        os.chdir(home+"/spartan-zkinterface/")

        print("Generating proof for " + f)
        subprocess.run("./target/release/spzk verify --nizk "+home+"/out/zkif/"+f+".zkif "+home+"/out/zkif/"+f+".inp.zkif "+home+"out/zkif/"+f+".wit.zkif", shell=True)


def run_dir(direc_path, ty, home):
        for f in os.listdir(direc_path):
            run_file(f, direc_path, ty, home)

def parse_lp(home, size, ty, custom=None): #TODO Lef
        if size == Size.SMALL:
                print("Running LP small Otti dataset")
                direc = os.fsencode(home+"/datasets/LP/MPS-small/")
                run_dir(direc,Type.LP,home)

        elif size == Size.FULL:
                print("Running LP full Otti dataset")
                direc = os.fsencode(home+"/datasets/LP/MPS-full/")
                run_dir(direc,Type.LP,home)

        elif custom != None:
                print("Running LP custom dataset")
                run_file(custom,Type.LP,home);

        else:
                print("Dataset for LP not specified, running small Otti dataset")
                direc = os.fsencode(home+"/datasets/LP/MPS-small/")
                run_dir(direc,Type.LP,home)

def parse_sdp(home, size, ty, custom=None):
        if size == Size.SMALL:
                print("Running SDP small Otti dataset")
                direc = os.fsencode(home+"/datasets/SDP/small/")
                run_dir(direc,Type.SDP,home)

        elif size == Size.FULL:
                print("Running SDP full Otti dataset, WARNING: do not attempt this without a lot of RAM")
                direc = os.fsencode(home+"/datasets/SDP/full/")
                run_dir(direc,Type.SDP,home)

        elif custom != None:
                print("Running SDP custom dataset")
                run_file(custom,Type.SDP,home);

        else:
                print("Dataset for SDP not specified, running small Otti dataset")
                direc = os.fsencode(home+"/datasets/SDP/small/")
                run_dir(direc,Type.SDP,home)

def run_sgd():
    print("Placeholder")


def parse_sgd(home,size,ty,custom=None): 
    print("Placeholder")




if __name__ == "__main__":
    home = os. getcwd() 

    parser = argparse.ArgumentParser()

    group = parser.add_mutually_exclusive_group()
    group.add_argument("--small", action='store_true')
    group.add_argument("--full", action='store_true')
    group.add_argument("--custom", type=str)

    parser.add_argument("--lp", action='store_true')
    parser.add_argument("--sdp", action='store_true')
    parser.add_argument("--sgd", action='store_true')
    args = parser.parse_args()

    subprocess.run(["mkdir", home+"/out"])
    subprocess.run(["mkdir", home+"/out/cfiles"])
    subprocess.run(["mkdir", home+"/out/zkif"])

    size = -1;
    if args.small:
        size = Size.SMALL;

    if args.full:
        size = Size.FULL;

    if args.lp:
        parse_lp(home,size,args,custom)

    if args.sdp:
        parse_sdp(home,size,args.custom)



