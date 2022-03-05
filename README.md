Otti
------
A zkSNARK compiler, solver, prover and verifier for optimization problems ([paper](https://eprint.iacr.org/2021/1436)).

# Cloning
To clone this repository and its submodules run
```
git clone --recurse-submodules git@github.com:eniac/otti.git
```

# Building
First, make sure you have installed [Docker CE](https://docs.docker.com/get-docker/).

Note: it is likely you will need to increase the memory limits for Docker on
your system.

Then build the Otti container
```
docker build -t otti .
```

And then get terminal access to it.
```
docker run -it otti
```

## Reproducing experimental results

After connecting to the Docker container, run the following script to reproduce the experimental results from [Otti](https://eprint.iacr.org/2021/1436).
```
./run.py [--lp | --sdp | --sgd] [--small | --full | --custom datasets/<path to dataset>]
```

One of the `--lp | --sdp | --sgd` options is required. Then either execute with
the `--small` or `--full` flag or `--custom` and explicitly give a path to a dataset file.

### Running the small suite
A subset of each dataset that can be reproduced on a personal computer with x86_64 architecture and > 4GB of RAM.
These datasets are expected to take less than 1 hour.


### Running the full suite
A subset of each MPS dataset that can be reproduced on a large machine with x86_64 architecture and > 512GB RAM.
These datasets can take several hours, in the order of 2-3 days to terminate. If your computer does not have sufficient
RAM memory or more applications have reserved memory, this might be killed by the OS. This is a well-known limitation
of the compiler that consumes large amounts of memory.


### Running individual files in `datasets/*`
Our script will generate a C-file from the dataset file including non-deterministic checks. We
compile it with the Otti compiler, prove and verify it and print `Verification successful` and the total runtime.
of each stage.


# Directory structure
```
├── compiler             -- zkSNARK compiler written in Haskell, a fork of CirC.
├── codegen              -- Code generators convert LP, SDP, SGD datasets to C files with non-deterministic checkers
├── datasets             -- The datasets from the Otti paper evaluation
│   ├── LP               -- Linear programming (LP) dataset based on Netlib-LP
│   ├── SDP              -- Semi-definite programming (SDP) dataset
│   ├── SGD              -- Stochastic Gradient Descent (SGD) dataset references and hyperparameters for PMLB dataset
├── Dockerfile           -- The Dockerfile that builds Otti
├── README.md            -- This file
├── run.py               -- Entry point executable script for reproducing experimental results (artifact eval)
├── Spartan              -- The Spartan zkSNARK prover/verifier back-end from Microsoft
├── spartan-zkinterface  -- A compatibility layer between the compiler and Spartan
└── deps                 -- External dependensieshttps://github.com/circify/circ
```


