#!/usr/bin/env python3
import itertools
import numpy as np
import sys
import math
import smcp
from smcp import SDP
import os
import sys
import subprocess

def mod_sol(probfile, solfile):
    copylines = ['"modified by Otti\n']

    with open(probfile, 'r') as f:
        c = 0
        for line in f.readlines():
            if not (line.startswith('"') or line.startswith('*')):
                copylines = copylines + [line]
                c = c+1
            if c >= 3: break

    cont = 1
    with open(solfile, 'r') as f:   
        content = f.readlines();
        if "Otti" in content[0]:
            cont = 0
        content = copylines + content

    if cont:
        with open("temp_"+solfile, 'w') as f:
            for line in content:
                f.write('%s' % line)

        os.rename("temp_"+solfile,solfile);


def parse_problem_file(probfile, solfile):
    Prob = SDP(probfile)

    n = Prob.n
    m = Prob.m
    
    B = np.zeros(m)
    for i in range(m):
        B[i] = Prob.b[i]

    A0 = Prob.get_A(0)
    C = np.zeros([n,n])
    for a in range(n):
            for b in range(n):
                if (a <= b): #lower t
                   C[a][b] = A0[a*n+b]
                   C[b][a] = A0[a*n+b]

    A = []
    for i in range(1,m+1):
        Ai = Prob.get_A(i)
        Aim = np.zeros([n, n])
        for a in range(n):
            for b in range(n):
                if (a <= b): #lower t
                    Aim[a][b] = Ai[a*n+b]
                    Aim[b][a] = Ai[a*n+b]

        A.append(Aim)


    mod_sol(probfile,solfile)
            
    Sol = SDP(solfile)

    Y = np.zeros(m)
    for i in range(m):
        Y[i] = Sol.b[i]*-1

    X = np.zeros([n,n])
    S = np.zeros([n,n])

    A2 = Sol.get_A(2)
    A1 = Sol.get_A(1)

    for a in range(n):
            for b in range(n):
                if (a <= b): #lower t
                   S[a][b] = A1[a*n+b] * -1
                   S[b][a] = A1[a*n+b] * -1

                   X[a][b] = A2[a*n+b] * -1
                   X[b][a] = A2[a*n+b] * -1

    return {'ndim':n, 'mdim':m, 'C':C, 'A':A, 'b':B, 'X':X, 'y':Y, 'S':S}


if __name__ == "__main__":
    
    dats = sys.argv[1]
    ds = dats.split('/')
    sol = '/'.join(ds[:-1])+"/sol_"+ds[-1]

    #run csdp
    #subprocess.run(["csdp", dats, sol], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    
    #read in solution file
    P = parse_problem_file(dats, sol)

    #subprocess.run("rm "+sol, shell=True)
    
    C = P['C']
    X = P['X']
    A = P['A']
    B = P['b']
    N = P['ndim'] # matrices n x n
    M = P['mdim'] # len of A list and b
    Y = P['y'] 
    S = P['S']

    np.set_printoptions(precision=15)

    XL = np.linalg.cholesky(X)

    SL = np.linalg.cholesky(S)

    Xf = []
    Yf = []
    XLf = []
    SLf = []
    for i in range(N):
        for j in range(N):
            Xf = Xf + [X[i][j]]
            if (i >= j): #low tri
                XLf = XLf + [XL[i][j]]
                SLf = SLf + [SL[i][j]]
    for i in range(M):
        Yf = Yf + [Y[i]]

    A0 = []
    A1 = []

    for i in range(N):
        for j in range(N):
            A0 = A0 + [A[0][i][j]]
            A1 = A1 + [A[1][i][j]]

    b0 = sum([ai*xi for (ai,xi) in zip(A0,Xf)])
    b1 = sum([ai*xi for (ai,xi) in zip(A1,Xf)])

#    print(Xf)
#    print(Yf)
#    print(XLf)
#    print(SLf)

    out = Xf+Yf+XLf+SLf
    for i in range(len(out)):
        if out[i] == -0.0:
            out[i] = 0.0

    print(out)
