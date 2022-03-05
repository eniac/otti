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
        Y[i] = Sol.b[i]

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
    sol = "sol_"+dats

    #run csdp
    #subprocess.run(["csdp", dats, sol])

    #read in solution file
    P = parse_problem_file(dats, sol)

    
    C = P['C']
    X = P['X']
    A = P['A']
    B = P['b']
    N = P['ndim'] # matrices n x n
    M = P['mdim'] # len of A list and b
    Y = P['y'] 
    S = P['S']

    XL = np.linalg.cholesky(X)

    SL = np.linalg.cholesky(S)

    print(XL)
    print(SL)

    XLf = []
    SLf = []
    for i in range(N):
        for j in range(N):
            if (i >= j): #low tri
                XLf = XLf + [XL[i][j]]
                SLf = SLf + [SL[i][j]]

    print(",".join(["%f" % x for x in XLf]) +",".join(["%f" % x for x in SLf]))

