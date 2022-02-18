import numpy as np
from sklearn.linear_model import SGDClassifier
from sklearn.datasets import make_blobs
from pmlb import fetch_data
import sys

d = int(sys.argv[1])
n = int(sys.argv[2])

inp = np.array([float(i) for i in input().split(',')])
assert(len(inp) % n == 0)
X = np.split(np.array(inp[:-n]), n)
y = inp[-n:]

def grad(clf, X, y, i):
  w = clf.coef_
  if y[i]*np.dot(w, X[i]) >= 1:
    return 0
  else:
    return 2*(y[i]*np.dot(w, X[i]) - 1) * y[i]*X[i]

def fit_and_check(X, y):
  clf = SGDClassifier(loss="squared_hinge", alpha = 0.0, eta0 = 1.0, learning_rate='adaptive', random_state=222, max_iter=80000, tol=0.001, fit_intercept=True)
  clf.fit(X, y)
  grads = [ np.linalg.norm(grad(clf, X, y, i)) for i in range(len(y)) ]
  S = sum(grads)
  if S == 0.0:
    return [ int(i) for i in clf.coef_.flatten() ]
  else:
    raise Exception("SGD check failed", grads)

def pmlb_fetch(dataset):
  data = fetch_data(dataset)
  Xa = np.array(data.loc[:, data.columns != 'target'])
  ya = 2 * np.array(data['target']) - 1.0 # Normalize to -1, 1
  return fit_and_check(Xa, ya)


# MAIN
# print(pmlb_fetch('clean1'))
# print(pmlb_fetch('clean2'))
print(fit_and_check(X, y))

