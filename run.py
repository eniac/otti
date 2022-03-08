#!/usr/bin/env python3
import subprocess
import argparse
import enum
import os
import json

class Size(enum.Enum):
    SMALL = 0
    FULL = 1

class Type(enum.Enum):
    LP = 0
    SDP = 1
    SGD = 2

def run_lp(): #TODO
    print("placeholder")

def parse_lp(home, size, custom=None): #TODO Lef
        if size == Size.SMALL:
                print("Running LP small Otti dataset")
                direc = os.fsencode(home+"/datasets/LP/MPS-small/")

        elif size == Size.FULL:
                print("Running LP full Otti dataset")
                direc = os.fsencode(home+"/datasets/LP/MPS-full/")

        elif custom != None:
                print("Running LP custom data")

        else:
                print("Dataset for LP not specified, running small Otti dataset")
                direc = os.fsencode(home+"/datasets/LP/MPS-small/")

def run_sdp(f, path, home):
        if not f.endswith('.dat-s'):
            print("ERROR: "+f+ " is not a dat-s file")
            return
    
        print("Making c file for " + f)
        subprocess.run(["python3", home+"/codegen/sdpcodegen.py", path+f])
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

        subprocess.run("./target/release/spzk verify --nizk "+home+"/out/zkif/"+f+".zkif "+home+"/out/zkif/"+f+".inp.zkif "+home+"/out/zkif/"+f+".wit.zkif", shell=True)


def parse_sdp(home, size, custom=None):
        if size == Size.SMALL:
                print("Running SDP small Otti dataset")
                direc = os.fsencode(home+"/datasets/SDP/small/")
                for name in os.listdir(direc):
                    f = name.decode('UTF-8')
                    path = direc.decode('UTF-8')
                    run_sdp(f, path, home)

        elif size == Size.FULL:
                print("Running SDP full Otti dataset, WARNING: do not attempt this without a lot of RAM")
                direc = os.fsencode(home+"/datasets/SDP/full/")
                for name in os.listdir(direc):
                    f = name.decode('UTF-8')
                    path = direc.decode('UTF-8')
                    run_sdp(f, path, home)

        elif custom != None:
                print("Running SDP custom data")
                
                abspath = os.path.abspath(custom)
                name = os.path.basename(custom)
                path = abspath[:(-1*len(name))]
                run_sdp(name, path, home)

        else:
                print("Dataset for SDP not specified, running small Otti dataset")
                direc = os.fsencode(home+"/datasets/SDP/small/")
                for name in os.listdir(direc):
                    f = name.decode('UTF-8')
                    path = direc.decode('UTF-8')
                    run_sdp(f, path, home)


def run_sgd(home, cfile, wfile, dataset, c1, c2, seed, eta0, maxiter, tol):

    subprocess.run(["python3", home+"/codegen/sgdcodegen.py", cfile, wfile,dataset, c1, c2, seed, eta0, maxiter, tol])
    subprocess.run(["mv", cfile, home+"/out/cfiles/"])
    subprocess.run(["mv", wfile, home+"/out/wit/"])

    print("Compile, solve, and prove " + dataset)
    os.chdir(home+"/rust-circ/")
    subprocess.run("./target/release/examples/circ --inputs "+home+"/out/wit/"+wfile+" "+home+"/out/cfiles/"+cfile+" r1cs --action spartan", shell=True)


def parse_sgd(home,size,custom=None): 
        if size == Size.SMALL:
                print("Running SGD small Otti dataset")
                json_file = (home+"/datasets/SGD/pmlb-small.json")
 
        elif size == Size.FULL:
                print("Running SGD full Otti dataset")
                json_file = (home+"/datasets/SGD/pmlb-small.json")

        elif custom != None:
                print("SGD custom data not available")
                return
        else:
            
                print("Dataset for SDP not specified, running small Otti dataset")
                json_file = (home+"/datasets/SGD/pmlb-small.json")

        with open(json_file) as f:
            data = json.load(f)
            
            for dataset in data: 
                cfile = dataset+".c"
                wfile = dataset+".in"

                run_sgd(home, cfile, wfile, dataset,\
                        str(data[dataset]["classes"][0]),\
                        str(data[dataset]["classes"][1]),\
                        str(data[dataset]["seed"]), str(data[dataset]["eta0"]),\
                        str(data[dataset]["maxiter"]), str(data[dataset]["tol"]))

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

    if not os.path.isdir(home+"/out"):
        subprocess.run(["mkdir", home+"/out"])
    if not os.path.isdir(home+"/out/cfiles"):
        subprocess.run(["mkdir", home+"/out/cfiles"])
    if not os.path.isdir(home+"/out/zkif"):
        subprocess.run(["mkdir", home+"/out/zkif"])
    if not os.path.isdir(home+"/out/wit"):
        subprocess.run(["mkdir", home+"/out/wit"])
    
    size = -1;
    if args.small:
        size = Size.SMALL;

    if args.full:
        size = Size.FULL;

    if args.lp:
        parse_lp(home,size,args.custom)
        subprocess.run(["rm", home+"/compiler/C",  home+"/compiler/x", home+"/compiler/w"])

    elif args.sdp:
        parse_sdp(home,size,args.custom)
        subprocess.run(["rm", home+"/compiler/C",  home+"/compiler/x", home+"/compiler/w"])

    elif args.sgd:
        parse_sgd(home,size,args.custom)

    else:
        parser.print_help()


