Otti
------
A zkSNARK compiler, solver, prover and verifier for optimization problems ([paper](https://eprint.iacr.org/2021/1436)).

# Cloning
To clone this repository and its submodules run
```
git clone https://github.com/eniac/otti.git
git checkout testcases
git submodule update --init
```

# Building with Packer

Install Packer

Ensure that you have the Amazon Web Services Command Line Interface (aws) installed.


Set up credentials for aws authorization

```
  export AWS_ACCESS_KEY_ID=$(aws configure get aws_access_key_id)
  export AWS_SECRET_ACCESS_KEY=$(aws configure get aws_secret_access_key)
```        

Run the Packer script 

```
packer build ami.pkr.hcl
```

At this point, Otti is installed in the VM and you are ready to use it.
Launch the VM, ssh into it, and call:

```
./generate_statements
```

It should produce the outputs in the `out` folder.

NOTE: The largest instances take a long time to generate and require
massive amounts of memory. Start small (with max size in config.json
on the order of 100 or so)
