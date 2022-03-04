import sys
import subprocess

dats = sys.argv[1]

#run csdp
subprocess.run(["csdp", dats, "sol_"++dats])

#read in solution file


#make XL, SL



