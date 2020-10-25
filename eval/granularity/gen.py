#!/usr/bin/env python3

"""
Generates a C program which asks whether some permuation on [N] has a preimage for 1.

The challenge is that the permutation is constructed as a *composition* of
permuations, each of which is an array lookup.

The generator includes the capability to pack multiple sub-permutations into
the same array, using offsets.
"""

from typing import List, NamedTuple, ByteString
from collections.abc import Iterable
from random import seed, shuffle
import hashlib
import functools as ft
import operator as op
import argparse as ap
import math

def bits(n: int) -> int:
    return 0 if n == 1 else 1 + bits(n // 2)

class PermCfg(NamedTuple):
    # Number of subpermuations in each array
    subperm_counts: List[int]
    # Size of each subperm
    subperm_size: int

class PRG(Iterable):
    state: ByteString

    def __init__(self, seed: int):
        self.state = str(seed).encode()

    def __next__(self):
        h = hashlib.sha256()
        h.update(self.state)
        self.state = h.digest()
        s = sum(c << (i * 8) for i, c in enumerate(self.state[:8]))
        return s

    def __iter__(self):
        return self

class Gen:
    output: List[str]
    # subperm group number
    ctr: int
    offset_var: str
    array_var_prefix: str

    def __init__(self, seed_: int = 0):
        seed(seed_)
        self.output = []
        self.ctr = 0
        self.offset_var = "i"
        self.array_var_prefix = "group"

    def start(self, w: int) -> None:
        i = self.offset_var
        self.output.append(f"int perm(int {i}) {{")
        self.output.append(f"  __VERIFIER_assume({i} >= 0 && {i} < {w});")

    def subperms(self, n: int, w: int) -> None:
        # Construct a bunch of random permutations
        perms = [list(range(0,w)) for r in range(n)]
        for p in perms:
            shuffle(p)
        # As a C curly-brace init
        perm_string = "{" + ", ".join(map(str,ft.reduce(op.add, perms))) + "}"
        array = f"{self.array_var_prefix}{self.ctr}"
        self.output.append(f"  int {array}[{n*w}] = {perm_string};")
        for i in range(n):
            self.output.append(f"  {self.offset_var} = {array}[{w * i} + {self.offset_var}];")
        self.ctr += 1

    def end(self) -> None:
        self.output.append("  __VERIFIER_assert(i != 0);")
        self.output.append("  return 0;")
        self.output.append("}")

def gen(cfg: PermCfg) -> str:
    output = ""
    g = Gen(0)
    g.start(cfg.subperm_size)
    for n in cfg.subperm_counts:
        g.subperms(n, cfg.subperm_size)
    g.end()
    return "\n".join(g.output)

def main():
    p = ap.ArgumentParser(description=__doc__)
    p.add_argument('groups', metavar='N', type=int, nargs='*', help='number of subpermutations in each group')
    p.add_argument('--size', '-w', required=True, metavar='W', type=int, help='number entries in each subpermutations')
    a = p.parse_args()
    cfg = PermCfg(a.groups, a.size)
    s = gen(cfg)
    print(s)

main()
