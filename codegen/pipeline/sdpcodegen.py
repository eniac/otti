#!/usr/bin/env python3
import itertools
import numpy as np
import sys
import math
import smcp
from smcp import SDP
import os

def dot(A,B):
    s = 0
    X = A*B
    return np.sum(X)

def prod_sum(X,Y):
    return np.matrix.sum(np.multiply(X,Y))


    '''
    with open(init_path, 'r') as f:
        content = [line.rstrip('\n').strip().split('=')[0] for line in f.readlines()]

        idx = 0
        for line in content:

            if (idx > 0):
                 vals = line.split()
                 n_mat = int(vals[0])
                 n_block = int(vals[1])
                 rel_i = int(vals[2])
                 rel_j = int(vals[3])
                 val = float(vals[4])

                 abs_i = block_base[n_block-1] + rel_i - 1
                 abs_j = block_base[n_block-1] + rel_j - 1

                 if n_mat == 2:
                     X[abs_i, abs_j] = val

            idx += 1

        print(X)
        '''

def mod_sol(probfile, solfile):
    copylines = []

    with open(probfile, 'r') as f:
        c = 0
        for line in f.readlines():
            if not (line.startswith('"') or line.startswith('*')):
                copylines = copylines ++ [line]
                c = c+1
            if c >= 3: break

    with open(solfile, 'r') as f:   
        content = f.readlines();
        content = copylines ++ content

    with open("temp_"+solfile, 'w') as f:
        for line in content:
                f.write('%s\n' % line)

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
    print(B)

    X = np.zeros([n,n])
    S = np.zeros([n,n])

    A0 = Sol.get_A(0)
    A1 = Sol.get_A(0)

    for a in range(n):
            for b in range(n):
                if (a <= b): #lower t
                   S[a][b] = A0[a*n+b]
                   S[b][a] = A0[a*n+b]

                   X[a][b] = A1[a*n+b]
                   X[b][a] = A1[a*n+b]

    return {'ndim':n, 'mdim':m, 'C':C, 'A':A, 'b':B, 'X':X, 'y':Y, 'S':S}


def linear_comb_matrices(y,A):

    #yl = y.T.tolist()[0]
  
    return sum([y_i * a_i for y_i, a_i in zip(y,A)])

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
    
        #TODO sort out lower tri

    P = parse_problem_file(sys.argv[1], sys.argv[2])

    #print(P)
    
    C = P['C']
    X = P['X']
    A = P['A']
    B = P['b']
    N = P['ndim'] # matrices n x n
    M = P['mdim'] # len of A list and b
    Y = P['Y'] 
    S = P['S']

    for i in range(M):
        s = 0.0
        for q in range(N):
            for w in range(N):
                s = s + (A[i][q,w] * X[q,w])
        ep = abs(s - B[i])
        print("CORRECT: " + str(ep < 1.0))


    print(C)
    print(X)
    C_flat = np.array(C).reshape(1,N*N)[0]
    print(C_flat)

    X_flat = np.array(X).reshape(1,N*N)[0]
    print(X_flat)

    # Read in the file
    with open('sdp_fease.c', 'r') as file :
      filedata = file.read()

    print("starting read in")

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

    for i in range(N*N):
        s = s + "fp64 xq"+str(i)+","
    for i in range(N*N-1):
        s = s + "fp64 sq"+str(i)+","
    s = s + "fp64 sq"+str(N*N-1)

    filedata = filedata.replace('$params', s) #TODO get rid

    s = ""
    
    #lower triangular
    for i in range(N):
        for j in range(N):
            if (i < j):
                s = s + "int xq"+str(i*N+j)+" = 0.0;\n)" 
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
                s = s + "int xq"+str(i*N+j)+" = 0.0;\n)"
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
        s = s + "fp64 x"+str(i)+" = 0;\n"
    filedata = filedata.replace('$xvars', s)

    s = ""
    for i in range(M):
        s = s + "fp64 y"+str(i)+" = 0;\n"
    filedata = filedata.replace('$yvars', s)

    s = ""
    for i in range(N*N):
        s = s + "fp64 xq"+str(i)+" = 0;\n"
    for i in range(N*N):
        s = s + "fp64 sq"+str(i)+" = 0;\n"
    filedata = filedata.replace('$hvars', s)

    s = ""
    s = str(N)+","+str(M)+","
    for i in range(N*N):
        s = s +str(C_flat[i])+","
    for i in range(N*N):
        s = s +str(X_flat[i])+","
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

    for i in range(N*N):
        s = s + "xq"+str(i)+","
    for i in range(N*N-1):
        s = s + "sq"+str(i)+","
    s = s + "sq"+str(N*N-1)
    filedata = filedata.replace('$seq2', s)


    s = ""
    for j in range(M):
        s = s + "fp64 dot_b"+str(j)+" = "
        for i in range((N*N)-1):
            s = s + "(a"+str(j)+"_"+str(i)+"*x"+str(i)+") + "
        s = s + "(a"+str(j)+"_"+str((N*N)-1)+"*x"+str((N*N)-1)+");\n"
    filedata = filedata.replace('$dot_calc', s)

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
 
    print("writing")

    # Write the file out again
    out = sys.argv[2] #.replace('.dat-s','') + "_circ.c"
    with open(out, 'w+') as file:
      file.write(filedata)
