#!/usr/bin/env python3
import itertools
import numpy as np
import sys

def dot(A,B):
    s = 0
    X = A*B
    return np.sum(X)

def parse_problem_file(path):

    mdim = -1
    nblocks = -1
    c = []
    F = []
    ndim = -1
    block_base = []


    with open(path, 'r') as f:
        content = [line.rstrip('\n').strip().split('=')[0] for line in f.readlines()]

        idx = 0
        for line in content:
            if not line.startswith('"'):
                line = line.replace(',',' ')
                if idx == 0:
                    mdim = int(line)
                elif idx ==1:
                    nblock = int(line)
                elif idx ==2:
                    block_struct = [abs(int(x)) for x in line.split()]
                    ndim = sum(block_struct)
                    for i in range(mdim+1):
                        F.append(np.mat(np.zeros((ndim,ndim))))

                    block_base = [sum(block_struct[:i]) for i in range(len(block_struct))]
                elif idx ==3:
                    c = [float(x) for x in line.split()]
                else:
                    vals = line.split()
                    n_mat = int(vals[0])
                    n_block = int(vals[1])
                    rel_i = int(vals[2])
                    rel_j = int(vals[3])
                    val = float(vals[4])

                    abs_i = block_base[n_block-1] + rel_i - 1
                    abs_j = block_base[n_block-1] + rel_j - 1

                    F[n_mat][abs_i,abs_j] = val
                    F[n_mat][abs_j,abs_i] = val

                idx += 1

        C = -F[0]
        b = [-x for x in c]
        A = [-x for x in F[1:]]

        return {'A':A, 'b':b, 'C':C, 'theta': 1.0, 'beta':0.25, 'nblock':nblock, 'block_struct':block_struct, 'block_base':block_base, 'mdim':mdim, 'ndim': ndim}

def det_name(front, rows, cols):
    list.sort(rows)
    list.sort(cols)
    s = front
    for r in rows:
        s = s + "_r" + str(r);
    for c in cols:
        s = s + "_c" + str(c);
    return s;

def calc_det(Q,name,name_store,rs,cs,n,s,x,extra):
    r, c = Q.shape
    if (r == 1):
        s = s + "solved = solved && ("+str(x)+str(Q[0][0])+" > -0.01);\n"
    elif (r == 2):
        s = s + "fp64 "+name+" = ("+str(x)+str(Q[0][0])+"*"+str(x)+str(Q[1][1])+") - ("+str(x)+str(Q[0][1])+"*"+str(x)+str(Q[1][0])+");\n"
        if (extra): s = s + "solved = solved && ("+name+" > -0.01);\n"
    else:
        w = "fp64 "+name+" = "
        for p in range(r-1):
            dn = det_name(name,rs+[(Q[0][0])],cs+[(Q[0][p])])
            s = check_name(dn,name_store,rs+[(Q[0][0])],cs+[(Q[0][p])],n,s,x)
            if (p % 2 == 0): #even

                w = w + "("+str(x)+str(Q[0][p])+" * "+dn+") + "
            else: #odd
                w = w + "("+str(x)+str(Q[0][p])+" * "+dn+" * -1) + "
        dn = det_name(name,rs+[(Q[0][0])],cs+[(Q[0][r-1])])
        s = check_name(dn,name_store,rs+[(Q[0][0])],cs+[(Q[0][r-1])],n,s,x)
        if ((r-1) % 2 == 0): #even
            w = w + "("+str(x)+str(Q[0][r-1])+" * "+dn+");\n"
        else: #odd
            w = w + "("+str(x)+str(Q[0][r-1])+" * "+dn+" * -1);\n"
        if (extra): w = w + "solved = solved && ("+name+" > -0.01);\n"
        s = s + w
    return s

def check_name(name,name_store,rs,cs,n,s,x):
    if (name in name_store):
        return s
    else:
        Q = np.array([x for x in range(n*n)]).reshape(n,n)
        for t in rs:
            Q = np.delete(Q, t, axis=0)
        for t in cs:
            Q = np.delete(Q, t, axis=1)
        s = calc_det(Q, name, name_store,rs,cs,n,s,x,0)
        return s


if __name__ == "__main__":

    if len(sys.argv) != 4:
        print("Usage: sdp_meta.py problem_file init_file output")
    else:
        P = parse_problem_file(sys.argv[1])
        parse_initial_point(sys.argv[2],P, sparse=False)

    C = P['C']
    X = P['X']
    A = P['A']
    B = P['b']

    print("C")
    print(C)
    print("X")
    print(X)
    print("A")
    print(A)
    print("b")
    print(b)


    N = 3 # matrices n x n
    M = 2 # len of A list and b

'''
    C = [[-0.99154214,  0.65386878, -0.64033738], [0.65386878,  0.93792596, -0.14210919], [-0.64033738, -0.14210919,  0.30795382]]
    B = [3.9888687923835366, -1.0823059466682765]
    X = [[1.78032993, -0.00358676,  0.72814533], [-0.00358676,  1.22372938, -0.05358303], [0.72814533, -0.05358303,  1.84385819]];

    A = [[[0.35184943, -0.38338125,  0.31362847], [-0.38338125,  0.85702573, -0.58909452], [0.31362847, -0.58909452,  0.97137504]], [[-0.4945009 , -0.62078468,  0.20384815], [-0.62078468,  0.21578012, -0.09717294], [0.20384815, -0.09717294, -0.42178768]]]



    C = [[-0.1983367,  0.54620727], [0.54620727,  0.29183634]]
    X = [[1.3713053, 0.16070848],[0.16070848, 1.43693619]]
    A = [[[-0.99890972, 0.14410886],[0.14410886, -0.73737868]],   [[0.14047667, -0.17714865],[-0.17714865,  0.49857682]]]

    B = [-2.3830572764539832, 0.8521208961278653]
    '''

    C_flat = np.array(C).reshape(1,N*N)[0]
    X_flat = np.array(X).reshape(1,N*N)[0]


    # Read in the file
    with open('sdp_temp.c', 'r') as file :
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
    for i in range(M-1):
        s = s + "fp64 y"+str(i)+","
    s = s + "fp64 y"+str(M-1)
    filedata = filedata.replace('$params', s)


    #det
    s = ""
    ind = np.array([x for x in range(N*N)]).reshape(N,N)
    name_store = []
    for k in range(N-1,-1,-1): #k
        print("k = " + str(k))
        combs = list(itertools.combinations([x for x in range(N)],k))
        list.reverse(combs)
        print(combs)

        for c in combs:
            c = sorted(c)
            list.reverse(c)
            print(c)
            rs = []
            cs = []
            Q = ind
            print(Q)
            for t in c:
                for a in range(2):
                    Q = np.delete(Q, t, axis=a)
                rs = rs + [t]
                cs = cs + [t]

            name = det_name("dx", rs, cs)
            name_store = name_store + [name]

            s = calc_det(Q, name, name_store,rs,cs,N,s,"x",1)

    filedata = filedata.replace('$det1', s)


    s = ""
    ind = np.array([x for x in range(N*N)]).reshape(N,N)
    name_store = []
    for k in range(N-1,-1,-1): #k
        print("k = " + str(k))
        combs = list(itertools.combinations([x for x in range(N)],k))
        list.reverse(combs)
        print(combs)

        for c in combs:
            c = sorted(c)
            list.reverse(c)
            print(c)
            rs = []
            cs = []
            Q = ind
            print(Q)
            for t in c:
                for a in range(2):
                    Q = np.delete(Q, t, axis=a)
                rs = rs + [t]
                cs = cs + [t]

            name = det_name("ds", rs, cs)
            name_store = name_store + [name]

            s = calc_det(Q, name, name_store,rs,cs,N,s,"s",1)

    filedata = filedata.replace('$det2', s)


    s = ""
    for j in range(M):
        s = s + "fp64 dot_s"+str(j)+"f = "
        for i in range((N*N)-1):
            s = s + "(a"+str(j)+"_"+str(i)+"*x"+str(i)+") + "
        s = s + "(a"+str(j)+"_"+str((N*N)-1)+"*x"+str((N*N)-1)+");\n"
        s = s + "solved = solved && d_equal(dot_s"+str(j)+"f,b"+str(j)+");\n"
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
    for i in range(M-1):
        s = s + "y"+str(i)+","
    s = s + "y"+str(M-1)
    filedata = filedata.replace('$seq2', s)



    # Write the file out again
    with open(output, 'w+') as file:
      file.write(filedata)
