#!/usr/bin/env python3
import itertools
import numpy as np
import sys
import math
import random
from pmlb import fetch_data
from sklearn.linear_model import SGDClassifier
from sklearn.datasets import make_classification

def grad(clf, X, y, i):
  w = clf.coef_
  if y[i]*np.dot(w, X[i]) >= 1:
    return 0
  else:
    return 2*(y[i]*np.dot(w, X[i]) - 1) * y[i]*X[i]

def fit_and_check(X, y,e,mi,t):
  clf = SGDClassifier(loss="squared_hinge",\
      alpha = 0.0, eta0 = e, learning_rate='adaptive',\
      max_iter=mi, tol=t, fit_intercept=True)
  clf.fit(X, y)
  grads = [ np.linalg.norm(grad(clf, X, y, i)) for i in range(len(y)) ]
  S = sum(grads)
  if S == 0.0:
    return [ int(i) for i in clf.coef_.flatten() ]
  else:
    return [] #raise Exception("SGD check failed", grads)

def random_gen(samples, features):
    
    r = []
    while r == []:
        rscale = random.randint(1,100)
        X,y = make_classification(n_samples=samples,n_features=features,n_redundant=0,scale=rscale)

        X = np.array(X).astype(int)
        y = np.array([(1 if yi == 1 else -1) for yi in y]).astype(int)
        
        r = fit_and_check(X, y,0.01,1000,0.001)
    
    return X,y,r

def pmlb_fetch(dataset,c1,c2,seed,eta0,maxiter,tol):
  data = fetch_data(dataset)

  X = np.array(data.loc[data['target'].isin([c1, c2]), data.columns != 'target']).astype(int)
  y = np.array(data.loc[data['target'].isin([c1, c2])]['target'].map(lambda x: 1 if x == c1 else -1)).astype(int)

  r = fit_and_check(X, y,seed,eta0,maxiter,tol)
  #Xn, Yn = pmlb_fetch(dataset, return_X_y=True)
  return X,y,r


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

    c_file = sys.argv[1]
    w_file = sys.argv[2]
    samples = int(sys.argv[3])
    features = int(sys.argv[4])
    #dataset_name = sys.argv[3]
    #c1 = int(sys.argv[4])
    #c2 = int(sys.argv[5])
    #seed = int(sys.argv[5])
    #eta0 = float(sys.argv[7])
    #maxiter = int(sys.argv[8])
    #tol = float(sys.argv[9])
    #prob = int(sys.argv[10])

    # make c with witnesses
    X, y, vals = random_gen(samples, features)
    print(X,y)

    n, d = X.shape
    for i in range(y.size):
        if (y[i] == 0):
            y[i] = -1

    # Read in the file
    C = """#include <stdbool.h>

bool main($existentials) {

    int neg = 1-2;

    $change

    $grad_checks

    bool check = $and_checks;

    return check;
}"""

    ws = ["{}{}".format("w", i) for i in range(d) ]
    WS =  ["{}{}".format("W",i) for i in range(d) ]
    xis = ["{}{}".format("x", i) for i in range(d) ]
    cs =  ["{}{}".format("c", i) for i in range(n) ]

    negs = ""
    j = 0
    for wi in vals:
      if (wi < 0):
        negs = negs + "\tw"+str(j)+" = neg * w"+str(j)+";\n"
      j = j+1

    substitutions = {
      'change' : negs,
      'existentials' : ", ".join(["__attribute__((private(0))) int {}".format(w) for w in ws]),
#          'dataset': ", ".join(["%d" % x for x in np.append(X.flatten() ,y)]),
#          'd': str(d),
#          'ws': ", ".join([ "long int " + w for w in ws]),
#          'xis': ", ".join([ "long int " + x for x in xis]),
#          'n': str(n),
#          'dot_wsxis' : dot(ws, xis),
      'grad_checks': "\n\t".join([ "bool c{} = {} *({})>=1;".format(i,y[i]," + ".join([wi + " * " + xi for wi, xi in zip(ws, ["%d" % x for x in X[i,:]])])) for i in range(n) ]),
      'and_checks': " && ".join(cs)
  }

    for (term, subst) in sorted(substitutions.items(), key=lambda k: -len(k[0])):
      C = C.replace("${}".format(term), subst)


    with open(c_file, 'w+') as f:
      f.write(C)

    wit = ""
    i = 0;
    for wi in vals:
      if (wi < 0): wi = wi * -1
      wit = wit + "w"+str(i)+" "+str(wi) + "\n"
      i = i + 1

    with open(w_file, 'w+') as f:
      f.write(wit)

