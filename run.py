#!/usr/bin/env python3
import subprocess

print("Placeholder")

SMALL = 0
FULL = 1


def run_sdp(dataset_path):
	for f in dataset_path:
		if f.endswith(".dat-s"):

			print("making c file for " ++ f)
			subprocess.run(["python3", "codegen/sdpcodegen.py", dataset_path+f])
			subprocess.run(["mv", f+".c", "out/cfiles/"])

			print("compiling R1CS for " ++ f)
			subprocess.run(["C_outputs="+f+".zkif", "stack", "run", "--", "c", "main", "out/cfiles/"+f, "--emit-r1cs"])
			subprocess.run(["C_outputs="+f+".zkif", "stack", "run", "--", "c", "main", "out/cfiles/"+f, "--prove", "-i", '<(echo "")'])
			subprocess.run(["mv", f+"inp.zkif", "out/zkif/"])
			subprocess.run(["mv", f+"wit.zkif", "out/zkif/"])
			subprocess.run(["mv", f+".zkif", "out/zkif/"])
	
			print("generating proof for " ++ f)
			subprocess.run(["", "", ""])
		else:
			print(f++" is not a .dat-s" file)


def parse_sdp(size=-1, custom=NULL):
	if size == SMALL:
		print("running SDP small Otti dataset")
		run_sdp("datasets/SDP/small/");

	elif size == FULL:
		print("running SDP full Otti dataset, WARNING: do not attempt this without a lot of RAM")
		run_sdp("datasets/SDP/full/");

	elif custom != NULL:
		print("running SDP custom dataset")
#ends with /?
		run_sdp("");

	else:
		print("dataset for SDP not specified, running small Otti dataset")
		run_sdp("datasets/SDP/small/");



