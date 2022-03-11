PMLB dataset
--------------

The PMLB dataset ships as a python package(https://pypi.org/project/pmlb/). Here we include JSON files
listing datasets that we tested on, as well as hyperparameters for training our SGD classifier on these
datasets. There are three JSON files.
1. PMLB-small.json
2. PMLB-full.json
3. PMLB-prob.json

## PMLB-small
A subset of the PMLB dataset that works with our SGD classifier using the "square hinge loss" function and
can also be trained on a personal computer, x86_64 architecture and > 12GB of RAM.

## PMLB-full
A larger subset of the PMLB dataset that works with our SGD classifier using the "square hinge loss" function
and can only be trained on a large machine, x86_64 architecture and > 200GB RAM.

## PMLB-prob
This includes PMLB datasets we created probabilstic certificates for. Running
the --full experiement will also run these datasets.

