#!/usr/bin/env python3
import itertools
import numpy as np
import sys
import math
from pmlb import fetch_data

def slice(arr, d = None, n = None):
  N, D = arr.shape
  if n == None:
    n = N
  if d == None:
    d = D
  assert(N >= n and D >= d)
  return arr[:n, :d]

def dot(a, b):
  assert(len(a) == len(b))
  out = []
  for x,y in zip(a, b):
    out += ["{} * {}".format(x, y)]

  return " + ".join(out)

if __name__ == "__main__":

    X, y = fetch_data(sys.argv[1], return_X_y=True)
    # X, y = slice(Xorig, 4, 3), yorig[:3]
    n, d = X.shape
    for i in range(y.size):
        if (y[i] == 0):
            y[i] = -1

    # Read in the file
    with open('sgd_temp.c', 'r') as file :

      C = file.read()
      ws = ["{}{}".format("w", i) for i in range(d) ]
      WS =  ["{}{}".format("W",i) for i in range(d) ]
      xis = ["{}{}".format("x", i) for i in range(d) ]

      substitutions = {
          'existentials' : ", ".join(["{} = __GADGET_exist()".format(w) for w in WS]),
          'dataset': ", ".join(["%d" % x for x in np.append(X.flatten() ,y)]),
          'd': str(d),
          'ws': ", ".join([ "long int " + w for w in ws]),
          'xis': ", ".join([ "long int " + x for x in xis]),
          'n': str(n),
          'dot_wsxis' : dot(ws, xis),
          'grad_checks': ",\n\t".join([ "grad_check({}, {}, {})".format(", ".join(WS), ", ".join(["%d" % x for x in X[i, :]]), y[i]) for i in range(n) ])
      }

      for (term, subst) in sorted(substitutions.items(), key=lambda k: -len(k[0])):
        C = C.replace("${}".format(term), subst)

      print(C)


