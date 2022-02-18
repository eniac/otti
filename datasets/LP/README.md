Netlib-LP dataset
--------------

The netlib-LP dataset is distributed as compressed MPS files (http://www.netlib.org/lp/data/).
The user must download the `emps` decompression program as C-source code, compile, fetch each dataset
and decompress. Fortunately, that work has been done already by us and all the MPS files here are
uncompressed and in plain-text. These are a subset of the MPS dataset that we tested Otti with,
some datasets where too big for Otti to terminate in a reasonable time.

To read more about the MPS file format (http://lpsolve.sourceforge.net/5.5/mps-format.htm).

There are two directories with MPS data
1. MPS-small
2. MPS-full

## MPS-small
A subset of the MPS dataset that can be reproduced on a personal computer,
x86_64 architecture and > 4GB of RAM.

## MPS-large
A subset of the MPS dataset that can be reproduced on a large machine,
x86_64 architecture and > 512GB RAM.


