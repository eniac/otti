#!/usr/bin/env python3
import itertools
import numpy as np
import sys
import math
import smcp
from smcp import SDP
import os

def prod_sum(X,Y):
    return np.matrix.sum(np.multiply(X,Y))


def parse_problem_file(probfile):
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


    return {'ndim':n, 'mdim':m, 'C':C, 'A':A, 'b':B}


def rand_mat(m,n):
    return 2*np.mat(np.random.rand(m,n)) - np.mat(np.ones((m,n)))

def rand_sym_mat(n):
    r = rand_mat(n,n)
    return 0.5*(r+r.T)

def rand_psd_mat(n):
    X = rand_mat(n,n)
    X = X.T*X

    return X

def rand_pd_mat(n):
    X = rand_psd_mat(n)
    X += np.mat(np.eye(n))
    return X


def random_problem(m,n):
    A = [rand_sym_mat(n) for i in range(m)]

    X = rand_pd_mat(n)

    b = [prod_sum(X,a) for a in A]
    C = rand_sym_mat(n)

    return {'A':A, 'X':X, 'b':b, 'C':C, 'theta': 1.0, 'beta':0.25}

if __name__ == "__main__":
    
    dats = sys.argv[1]

    P = parse_problem_file(dats)

    C = P['C']
    A = P['A']
    B = P['b']
    N = P['ndim'] # matrices n x n
    M = P['mdim'] # len of A list and b

    C_flat = np.array(C).reshape(1,N*N)[0]
    #print(C_flat)


    # Read in the file
    with open('sdp_feas.c', 'r') as file :
      filedata = file.read()

    s = ""
    for i in range(N*N):
        s = s + "fp64 c"+str(i)+","
    for i in range(N*N):
        s = s + "fp64 x"+str(i)+","
    for i in range(M):
        for j in range(N*N):
            s = s + "fp64 a"+str(i)+"_"+str(j)+","
    for i in range(M):
        s = s + "fp64 b"+str(i)+","
    for i in range(M):
        s = s + "fp64 y"+str(i)+","
    for i in range(N):
        for j in range(N):
            if (i >= j):
                s = s + "fp64 xq"+str(i*N+j)+","
    for i in range(N):
        for j in range(N):
            if (i >= j):
                 s = s + "fp64 sq"+str(i*N+j)+","
    s = s[:-1]
    filedata = filedata.replace('$params', s)

    s = ""
    #lower triangular
    for i in range(N):
        for j in range(N):
            if (i < j):
                s = s + "fp64 xq"+str(i*N+j)+" = 0.0;\n" 
                #s = s + "solved = solved && (d_equal(xq"+str(i*N+j)+",0.0));\n"
    
    #transpose
    for i in range (N):
        for j in range(N):
            s = s + "fp64 xr"+str(i*N+j)+" = xq"+str(j*N+i)+";\n"

    #mult
    for i in range(N):
        for j in range(N):
            s = s + "fp64 xm"+str(i*N+j)+" = "
            for k in range(N-1):
                s = s + "(xq"+str(i*N+k)+" * xr"+str(k*N+j)+") +"
            k = N-1;
            s = s + "(xq"+str(i*N+k)+" * xr"+str(k*N+j)+");\n"

    #PSD
    for i in range (N*N):
         s = s + "solved = solved && (d_equal(x"+str(i)+",xm"+str(i)+"));\n"

    filedata = filedata.replace('$chol1', s)


    s = ""
    
    #lower triangular
    for i in range(N):
        for j in range(N):
            if (i < j):
                s = s + "fp64 sq"+str(i*N+j)+" = 0.0;\n"
                #s = s + "solved = solved && (d_equal(sq"+str(i*N+j)+",0.0));\n"

    #transpose
    for i in range (N):
        for j in range(N):
            s = s + "fp64 sr"+str(i*N+j)+" = sq"+str(j*N+i)+";\n"

    #mult
    for i in range(N):
        for j in range(N):
            s = s + "fp64 sm"+str(i*N+j)+" = "
            for k in range(N-1):
                s = s + "(sq"+str(i*N+k)+" * sr"+str(k*N+j)+") +"
            k = N-1;
            s = s + "(sq"+str(i*N+k)+" * sr"+str(k*N+j)+");\n"

    #PSD
    for i in range (N*N):
         s = s + "solved = solved && (d_equal(s"+str(i)+",sm"+str(i)+"));\n"

    filedata = filedata.replace('$chol2', s)




    s = ""
    for j in range(M):
        s = s + "solved = solved && d_equal(dot_b"+str(j)+",b"+str(j)+");\n"
    filedata = filedata.replace('$a_x', s)

    s = ""
    for i in range(N*N):
        s = s + "fp64 s"+str(i)+" = c"+str(i)+" - ("
        for j in range(M-1):
            s = s + "(a"+str(j)+"_"+str(i)+" * y"+str(j)+") + "
        s = s + "(a"+str(M-1)+"_"+str(i)+" * y"+str(M-1)+"));\n"
    filedata = filedata.replace('$s_mat', s)

    s = ""
    for i in range((N*N)-1):
        s = s + "(s"+str(i)+"*x"+str(i)+") + "
    s = s + "(s"+str((N*N)-1)+"*x"+str((N*N)-1)+")"
    filedata = filedata.replace('$gap', s)

    s = ""
    for i in range(N*N):
        s = s + "fp64 x"+str(i)+" = __GADGET_exist();\n"
    filedata = filedata.replace('$xvars', s)

    s = ""
    for i in range(M):
        s = s + "fp64 y"+str(i)+" = __GADGET_exist();\n"
    filedata = filedata.replace('$yvars', s)

    s = ""
    for i in range(N):
        for j in range(N):
            if (i >= j):
                s = s + "fp64 xq"+str(i*N+j)+" = __GADGET_exist();\n"
    for i in range(N):
        for j in range(N):
            if (i >= j):
                s = s + "fp64 sq"+str(i*N+j)+" = __GADGET_exist();\n"
    filedata = filedata.replace('$lvars', s)

    s = ""
    s = str(N)+","+str(M)+',"'+dats+'",'
    for i in range(N*N):
        s = s +str(C_flat[i])+","
    for i in range(M):
        for j in range(N*N):
            s = s +str(np.array(A[i]).reshape(1,N*N)[0][j])+","
    for i in range(M-1):
        s = s +str(B[i])+","
    s = s + str(B[M-1])

    filedata = filedata.replace('$seq1', s)

    s = ""
    s = s +str(N)+","+str(M)+","
    for i in range(N*N):
        s = s +str(C_flat[i])+","
    for i in range(N*N):
        s = s + "x"+str(i)+","
    for i in range(M):
        for j in range(N*N):
            s = s +str(np.array(A[i]).reshape(1,N*N)[0][j])+","
    for i in range(M):
        s = s +str(B[i])+","
    for i in range(M):
        s = s + "y"+str(i)+","

    for i in range(N):
        for j in range(N):
            if (i >= j):
                s = s + "xq"+str(i*N+j)+","
    for i in range(N):
        for j in range(N):
            if (i >= j):
                 s = s + "sq"+str(i*N+j)+","
    s = s[:-1]
    filedata = filedata.replace('$seq2', s)

    s = ""
    for j in range(M):
        s = s + "fp64 dot_b"+str(j)+" = "
        for i in range((N*N)-1):
            s = s + "(a"+str(j)+"_"+str(i)+"*x"+str(i)+") + "
        s = s + "(a"+str(j)+"_"+str((N*N)-1)+"*x"+str((N*N)-1)+");\n"
    filedata = filedata.replace('$dot_calc', s)

    ''' INFEASIBLILITY
    s = ""
    for i in range(M):
        s = s + "solved = solved || (dot_b"+str(i)+" != b"+str(i)+");\n"
    filedata = filedata.replace('$infeas', s)

    s = ""

    for e in range(N):
        for i in range(N):
            s = s + "fp64 l"+str(e)+"_"+str(i)+" = (sq"+str(e)+" * xq"+str(i*N+e)+");\n"

        for i in range(N):
            s = s + "fp64 r"+str(e)+"_"+str(i)+" = "
            for k in range(N-1):
                s = s + "(x"+str(i*N+k)+" * xq"+str(k*N+e)+") + "
            s = s + "(x"+str(i*N+(N-1))+" * xq"+str((N-1)*N+e)+");\n"

        #check is eigen and not positive

        s = s + "solved = solved || ("
        for i in range(N):
          s = s + "(l"+str(e)+"_"+str(i)+" == r"+str(e)+"_"+str(i)+") && "
        s = s + "(sq"+str(e)+" < 0.0));\n\n"

    filedata = filedata.replace('$eigen', s)
    '''

    # Write the file out again
    out = dats + ".c"
    with open(out, 'w+') as file:
      file.write(filedata)
