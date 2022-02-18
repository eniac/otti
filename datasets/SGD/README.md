PMLB dataset
--------------

The PMLB dataset ships as a python package(https://pypi.org/project/pmlb/). Here we include JSON files
listing datasets that we tested on, as well as hyperparameters for training our SGD classifier on these
datasets. There are two JSON files.
1. PMLB-small.json
2. PMLB-full.json

## PMLB-small
A subset of the PMLB dataset that works with our SGD classifier using the "square hinge loss" function and
can also be trained on a personal computer, x86_64 architecture and > 4GB of RAM.

## PMLB-large
A larger subset of the PMLB dataset that works with our SGD classifier using the "square hinge loss" function
and can only be trained on a large machine, x86_64 architecture and > 512GB RAM.

