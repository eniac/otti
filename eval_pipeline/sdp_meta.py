#!/usr/bin/env python3
N = 2 # matrices n x n
M = 2 # len of A list and b

BN = (N*N+M)
BM = (N*N+M)*(N*N+M)



def get(i, j, n):
  return (i*n+j)

matrix_init = "{"
for i in range(N*N - 1):
    matrix_init = matrix_init + "0,"
matrix_init = matrix_init + "0};\n"

vector_init_ne = "{"
for i in range(M - 1):
    vector_init_ne = vector_init_ne + "0,"
vector_init_ne = vector_init_ne + "0}};\n"

b_matrix_init = ""
for i in range(BN*BN - 1):
    b_matrix_init = b_matrix_init + "0,"
b_matrix_init = b_matrix_init + "0};\n"

vector_init = "{"
for i in range(M - 1):
    vector_init = vector_init + "0,"
vector_init = vector_init + "0};\n"

b_vector_init = ""
for i in range(BM - 1):
    b_vector_init = b_vector_init + "0,"
b_vector_init = b_vector_init + "0};\n"



# Read in the file
with open('sdp_temp.c', 'r') as file :
  filedata = file.read()



s = "\t#define N " + str(N) + "\n"
s = s+ "\t#define M " + str(M) + "\n"
filedata = filedata.replace('$macros', s)

filedata = filedata.replace('$v_init_ne', vector_init_ne)
filedata = filedata.replace('$m_init', matrix_init)
filedata = filedata.replace('$b_m_init', b_matrix_init)
filedata = filedata.replace('$v_init', vector_init)
filedata = filedata.replace('$b_v_init', b_vector_init)


n_elems = ""
for i in range(N*N):
    n_elems = n_elems + "fixed_point_precision_16_16 m" + str(i) + "; "
n_elems = n_elems + "\n"

m_elems = ""
for i in range(M):
    m_elems = m_elems + "fixed_point_precision_16_16 m" + str(i) + "; "
m_elems = m_elems + "\n"

bn_elems = ""
for i in range(BN*BN):
    bn_elems = bn_elems + "fixed_point_precision_16_16 m" + str(i) + "; "
bn_elems = bn_elems + "\n"

bm_elems = ""
for i in range(BM):
    bm_elems = bm_elems + "fixed_point_precision_16_16 m" + str(i) + "; "
bm_elems = bm_elems + "\n"

filedata = filedata.replace('$N_matrix_elems', n_elems)
filedata = filedata.replace('$B_matrix_elems', bn_elems)
filedata = filedata.replace('$M_vector_elems', m_elems)
filedata = filedata.replace('$B_vector_elems', bm_elems)

s = ""
for i in range(M):
    s = s + "\tMatrix m" + str(i) + "; "
s = s + "\n"
filedata = filedata.replace('$M_matrix_list', s)

s = ""
for i in range(M):
    s = s + "\tBig_Matrix m" + str(i) + "; "
s = s + "\n"
filedata = filedata.replace('$M_big_matrix_list', s)

s = ""
for i in range(N):
    for j in range(N):
        s = s + "\ts = s + (A.m" + str(get(i,j,N)) + " * B.m"+ str(get(i,j,N)) +");\n"
filedata = filedata.replace('$dot', s)

s = ""
for i in range(M):
    s = s + "\ts = s + (a.m" + str(i) + " * b.m"+ str(i) +");\n"
filedata = filedata.replace('$vec_comb', s)

s = ""
for i in range(N):
    s = s + "\tA.m" + str(i) + " = A.m"+ str(i) +" / s;\n"
filedata = filedata.replace('$scal_div', s)


s = ""
for i in range(N):
    for j in range(N):
        for k in range(N):
            s = s + "\tC.m" + str(get(i,j,N)) + " += A.m"+ str(get(i,j,N)) +" * B.m"+str(get(k,j,N))+";\n"
filedata = filedata.replace('$mat_mul', s)

s = ""
for i in range(BN):
    for j in range(BN):
        for k in range(BN):
            s = s + "\tC.m" + str(get(i,j,N)) + " += A.m"+ str(get(i,j,N)) +" * B.m"+str(get(k,j,N))+";\n"
filedata = filedata.replace('$b_mat_mul', s)

s = ""
for i in range(N):
    for k in range(N):
        s = s + "\tc.m" + str(i) + " += A.m"+ str(get(i,k,N)) +" * b.m"+str(k)+";\n"
filedata = filedata.replace('$vec_mul', s)

s = ""
for i in range(BN):
    for k in range(BN):
        s = s + "\tc.m" + str(i) + " += A.m"+ str(get(i,k,N)) +" * b.m"+str(k)+";\n"
filedata = filedata.replace('$b_vec_mul', s)

s = ""
for i in range(M):
    s = s + "\tc += a.m" + str(i) + " * b.m"+ str(i) +";\n"
filedata = filedata.replace('$vec_vec_mul', s)

s = ""
for i in range(N*N):
    s = s + "\tA.m" + str(i)+" = A.m" + str(i) + " * s;\n"
filedata = filedata.replace('$scal_mul', s)

s = ""
for i in range(N*N):
    s = s + "\tC.m" + str(i)+" = A.m" + str(i) + " + B.m"+ str(i) +";\n"
filedata = filedata.replace('$mat_add', s)

s = ""
for i in range(BN*BN):
    s = s + "\tC.m" + str(i)+" = A.m" + str(i) + " + B.m"+ str(i) +";\n"
filedata = filedata.replace('$b_mat_add', s)

s = ""
for i in range(N*N):
    s = s + "\tC.m" + str(i)+" = A.m" + str(i) + " - B.m"+ str(i) +";\n"
filedata = filedata.replace('$mat_sub', s)

s = ""
for i in range(BN*BN):
    s = s + "\tC.m" + str(i)+" = A.m" + str(i) + " - B.m"+ str(i) +";\n"
filedata = filedata.replace('$b_mat_sub', s)

s = ""
for i in range(1, M):
    s = s + "\tsum = mat_add(sum,scal_mul(A.m" + str(i)+", y.m" + str(i) + " ));\n"
filedata = filedata.replace('$mat_comb', s) #TODO CHECK

s = ""
for i in range(N):
    for j in range(N):
        s = s + "\tT.m" + str(get(i,j,N))+" = A.m" + str(get(j,i,N)) +";\n"
filedata = filedata.replace('$transpose', s)

s = ""
for i in range(M):
    s = s + "\tx.m" + str(i)+" = a.m" + str(i) + " - b.m"+ str(i) +";\n"
filedata = filedata.replace('$vec_sub', s)

s = ""
for i in range(BM):
    s = s + "\tx.m" + str(i)+" = a.m" + str(i) + " - b.m"+ str(i) +";\n"
filedata = filedata.replace('$b_vec_sub', s)

s = ""
for i in range(BM):
    s = s + "\tv.m" + str(i)+" = A.m" + str(i) + ";\n"
filedata = filedata.replace('$vectorize', s)

s = ""
for i in range(N*N):
    s = s + "\tA.m" + str(i)+" = P.m" + str(i) + ";\n"
filedata = filedata.replace('$biggify_mat', s)


#norm norm_circ
s = ""
for i in range(N*N + M):
    for k in range(N*N + M):
        s = s + "\tres.m" + str(i)+" += LL.m" + str(get(i,k,N*N+M)) + " * z.m"+str(k)+";\n"
for j in range(N*N+M):
    s = s + "\tres.m" + str(j)+" = e.m" + str(j) + " - res.m"+str(j)+";\n"
filedata = filedata.replace('$nc1', s)


s = ""
for i in range(BM):
    s = s + "\tsum = sum + pow_2(v.m" + str(i)+");\n"
filedata = filedata.replace('$norm_vec_big', s)


#norm mat circ
s = ""
for l in range(N):
    for g in range(N):
        if (l == g):
            s = s + "\tPA.m"+str(get(l,g,N))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$nmc1', s)


#swap
s = "\tfixed_point_precision_16_16 cmp; int max_ja;\n"
for y in range(N):
    s += "\tmax_ja = " + str(y)+";\n"
    for r in range(N):
        for sel in range(N):
            s += "\tif (max_ja == "+str(sel)+"){\tcmp = L.m"+str(get(sel,y,N))+";}\n"
        s += "\tif (abs_val(L.m"+str(get(r,y,N))+") > abs_val(cmp)){ max_ja = "+str(r)+"; }\n"
    for sel in range(N):
        s+= "if (max_ja == "+str(sel)+"){"
        if (sel != y):
            s += "\tfixed_point_precision_16_16 tempa"+str(y)+";"
            for sr in range(N):
                s += "\ttempa"+str(y)+" = PA.m"+str(get(y,sr,N))+";\n"
                s += "\tPA.m"+str(get(y,sr,N))+" = PA.m"+str(get(sel,sr,N))+";\n"
                s += "\tPA.m"+str(get(sel,sr,N))+" = tempa"+str(y)+";\n"
        s += "}\n"
filedata = filedata.replace('$swap1', s)



s = ""
for v in range(N):
    for j in range(N):
        for k in range(N):
            s = s + "\tLLp.m"+str(get(v,j,(N)))+" = PA.m"+str(get(v,k,(N)))+" * L.m"+str(get(k,j,(N)))+";\n"
filedata = filedata.replace('$nmc2', s)

s = ""
for v in range(N):
    s = s + "\tLA.m"+str(get(v,v,(N)))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$nmc3', s)

s = "\tfixed_point_precision_16_16 se;\n"
for i in range(N):
    for j in range(N):
        if (i<=j):
            s = s + "\tse = 0;\n"
            for c in range(j):
                s = s + "\tse += LA.m"+str(get(j,c,(N)))+" * UA.m"+str(get(c,i,(N)))+";\n"
            s = s + "\tUA.m"+str(get(j,i,(N)))+" = LLp.m"+str(get(j,i,(N)))+" - se;\n"
        if (j >= i):
            s = s + "\tse = 0;\n"
            for k in range(i):
                s = s + "\tse += LA.m"+str(get(j,k,(N)))+" * UA.m"+str(get(k,i,(N)))+";\n"
            s = s + "\tLA.m"+str(get(j,i,(N)))+" = (LLp.m"+str(get(j,i,(N)))+" - se) / UA.m"+str(get(i,i,(N)))+";\n"
filedata = filedata.replace('$nmc4', s)

s = ""
for t in range(N):
    for u in range(N):
        if (t==u):
            s = s + "\tIDT.m"+str(get(t,u,(N)))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$nmc5', s)

s = ""
for t in range(N):
    for u in range(N):
        if (t==u):
            s = s + "\tLU.m"+str(get(t,u,(N)))+" = LA.m"+str(get(t,u,(N)))+" + UA.m"+str(get(t,u,(N)))+" - IDT.m"+str(get(t,u,(N)))+";\n"
filedata = filedata.replace('$nmc6', s)

s = ""
for i in range(N):
    s += "Big_Vector bb"+str(i)+" = {N, " + b_vector_init
    for j in range(N):
        if (i==j):
            s = s + "\tbb"+str(i)+".m"+str(j)+" = (fixed_point_precision_16_16)1.0;\n"
    s += "Big_Vector b"+str(i)+" = {N, " + b_vector_init
    for i3 in range(N):
        for k3 in range(N):
            s = s + "\tb"+str(i)+".m"+str(i3)+" += P.m"+str(get(i3,k3,N))+" * bb"+str(i)+".m"+str(k3)+";\n"
    for d3 in range(N-1):
        for e3 in range(d3+1, N):
            s = s + "\tb"+str(i)+".m"+str(e3)+" = b"+str(i)+".m"+str(e3)+" - LU.m"+str(get(e3,d3,N))+" * b"+str(i)+".m"+str(d3)+";\n"
    for f3 in range(N-1,0,-1):
        s = s + "\tb"+str(i)+".m"+str(f3)+" = b"+str(i)+".m"+str(f3)+" / LU.m"+str(get(f3,f3,N))+";\n"
        for h3 in range(0,f3):
            s = s + "\tb"+str(i)+".m"+str(h3)+" = b"+str(i)+".m"+str(h3)+" - LU.m"+str(get(h3,f3,N))+" * b"+str(i)+".m"+str(f3)+";\n"
    for k4 in range(N):
        s = s + "\tLI.m"+str(get(k4,i,N))+" = b"+str(i)+".m"+str(k4)+";\n"
filedata = filedata.replace('$nmc7', s)

s = ""
for i in range(N):
    for j in range(N):
        if (t==u):
            s = s + "\tLT.m"+str(get(i,j,(N)))+" = L.m"+str(get(j,i,N))+";\n"
filedata = filedata.replace('$nmc8', s)

s = ""
for l in range(N):
    for g in range(N):
        if (l == g):
            s = s + "\tPB.m"+str(get(l,g,N))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$nmc9', s)

#swap
s = "\tfixed_point_precision_16_16 cmp2; int max_j2;\n"
for y in range(N):
    s += "\tmax_j2 = " + str(y)+";\n"
    for r in range(N):
        for sel in range(N):
            "\tif (max_j2 == "+str(sel)+"){\tcmp2 = LT.m("+str(get(sel,y,N))+";}\n"
        s += "\tif (abs_val(LT.m"+str(get(r,y,N))+") > abs_val(cmp2)){ max_j2 = "+str(r)+"; }\n"
    for sel in range(N):
        s+= "if (max_j2 == "+str(sel)+"){"
        if (sel != y):
            s += "\tfixed_point_precision_16_16 temp2"+str(y)+";\n"
            for sr in range(N):
                s += "\ttemp2"+str(y)+" = PB.m"+str(get(y,sr,N))+";\n"
                s += "\tPB.m"+str(get(y,sr,N))+" = PB.m"+str(get(sel,sr,N))+";\n"
                s += "\tPB.m"+str(get(sel,sr,N))+" = temp2"+str(y)+";\n"
        s += "}\n"
filedata = filedata.replace('$swap2', s)



s = ""
for v in range(N):
    for j in range(N):
        for k in range(N):
            s = s + "\tLLpp.m"+str(get(v,j,(N)))+" = PB.m"+str(get(v,k,(N)))+" * LT.m"+str(get(k,j,(N)))+";\n"
filedata = filedata.replace('$y1', s)

s = ""
for v in range(N):
    s = s + "\tLB.m"+str(get(v,v,(N)))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$y2', s)

s = "\tfixed_point_precision_16_16 se2;\n"
for i in range(N):
    for j in range(N):
        if (i<=j):
            s = s + "\tse2 = 0;\n"
            for c in range(j):
                s = s + "\tse2 += LB.m"+str(get(j,c,(N)))+" * UB.m"+str(get(c,i,(N)))+";\n"
            s = s + "\tUB.m"+str(get(j,i,(N)))+" = LLpp.m"+str(get(j,i,(N)))+" - se2;\n"
        if (j >= i):
            s = s + "\tse2 = 0;\n"
            for k in range(i):
                s = s + "\tse2 += LB.m"+str(get(j,k,(N)))+" * UB.m"+str(get(k,i,(N)))+";\n"
            s = s + "\tLB.m"+str(get(j,i,(N)))+" = (LLp.m"+str(get(j,i,(N)))+" - se2) / UB.m"+str(get(i,i,(N)))+";\n"
filedata = filedata.replace('$y3', s)

s = ""
for t in range(N):
    for u in range(N):
        if (t==u):
            s = s + "\tLU2.m"+str(get(t,u,(N)))+" = LB.m"+str(get(t,u,N))+" + UB.m"+str(get(t,u,N))+" - IDT.m"+str(get(t,u,N))+";\n"
filedata = filedata.replace('$y4', s)

s = ""
for i in range(N):
    s += "Big_Vector bba"+str(i)+" = {N, " + b_vector_init
    for j in range(N):
        if (i==j):
            s = s + "\tbba"+str(i)+".m"+str(j)+" = (fixed_point_precision_16_16)1.0;\n"
    s += "Big_Vector ba"+str(i)+" = {N, " + b_vector_init
    for i3 in range(N):
        for k3 in range(N):
            s = s + "\tba"+str(i)+".m"+str(i3)+" += PB.m"+str(get(i3,k3,N))+" * bba"+str(i)+".m"+str(k3)+";\n"
    for d3 in range(N-1):
        for e3 in range(d3+1, N):
            s = s + "\tba"+str(i)+".m"+str(e3)+" = ba"+str(i)+".m"+str(e3)+" - LU2.m"+str(get(e3,d3,N))+" * ba"+str(i)+".m"+str(d3)+";\n"
    for f3 in range(N-1,0,-1):
        s = s + "\tba"+str(i)+".m"+str(f3)+" = ba"+str(i)+".m"+str(f3)+" / LU2.m"+str(get(f3,f3,N))+";\n"
        for h3 in range(0,f3):
            s = s + "\tba"+str(i)+".m"+str(h3)+" = ba"+str(i)+".m"+str(h3)+" - LU2.m"+str(get(h3,f3,N))+" * ba"+str(i)+".m"+str(f3)+";\n"
    for k4 in range(N):
        s = s + "\tLTI.m"+str(get(k4,i,N))+" = ba"+str(i)+".m"+str(k4)+";\n"
filedata = filedata.replace('$y5', s)

s = ""
for v in range(N):
    for j in range(N):
        for k in range(N):
            s = s + "\tLID.m"+str(get(v,j,(N)))+" = LI.m"+str(get(v,k,(N)))+" * D.m"+str(get(k,j,(N)))+";\n"
filedata = filedata.replace('$y6', s)

s = ""
for v in range(N):
    for j in range(N):
        for k in range(N):
            s = s + "\tI.m"+str(get(v,j,(N)))+" = LID.m"+str(get(v,k,(N)))+" * LTI.m"+str(get(k,j,(N)))+";\n"
filedata = filedata.replace('$y7', s)










#####


s = ""
for i in range(N):
    for j in range(N):
        s = s + "\tsum = sum + pow_2(X.m" + str(get(i,j,N))+");\n"
filedata = filedata.replace('$norm_mat', s)



p = "\tfixed_point_precision_16_16 s;\n"
for i in range(N):
  for j in range(i+1):
    p = p + "\ts = 0.0;\n"

    for k in range(j):
      p = p + "\ts = s + R.m"+str(get(i,k,N))+" * R.m"+str(get(j,k,N))+";\n"

    if (i == j):
      p = p + "\tfixed_point_precision_16_16 to_roota"+str(i)+" = X.m"+str(get(i,i,N))+" - s;\n" + \
      "\tif (to_roota"+str(i)+" < (fixed_point_precision_16_16)(0.0)){\n" + \
        "\tr = 0;\n" + \
      "\t}\n" + \
      "\tR.m"+str(get(i,j,N))+" = sqrt_val(to_roota"+str(i)+");\n"

    else:
      p = p + "\tR.m"+str(get(i,j,N))+" = (fixed_point_precision_16_16)(1.0) / R.m"+str(get(j,j,N))+" * (X.m"+str(get(i,j,N))+" - s);"

filedata = filedata.replace('$psd', p)


s = ""
for i in range(M):
    s = s + " && (d_equal(dot(A.m"+str(i)+", Xp),b.m"+str(i)+"))"
filedata = filedata.replace('$s01', s)

p = "\tfixed_point_precision_16_16 s;\n"
for i in range(N):
    for j in range(i+1):
      p = p + "\ts = (fixed_point_precision_16_16)0.0;\n"

      for k in range(j):
        p = p + "\ts = s + L.m"+str(get(i,k,N))+" * L.m"+str(get(j,k,N))+";\n"

      if (i == j):
        p = p + "\tfixed_point_precision_16_16 to_root"+str(i)+" = Q.X.m"+str(get(i,i,N))+" - s;\n" + \
        "\tL.m"+str(get(i,j,N))+" = sqrt_val(to_root"+str(i)+");\n"
      else:
        p = p + "\tL.m"+str(get(i,j,N))+" = (fixed_point_precision_16_16)1.0 / L.m"+str(get(j,j,N))+" * (Q.X.m"+str(get(i,j,N))+" - s);\n"


filedata = filedata.replace('$s02', p)

s = ""
for v in range(N):
    for j in range(N):
        for k in range(N):
            s = s + "\tU1.m"+str(get(v,j,N))+" += Q.X.m"+str(get(v,k,N))+" * C.m"+str(get(k,j,N))+";\n"
filedata = filedata.replace('$s03', s)

s = ""
for v in range(N):
    for j in range(N):
        for k in range(N):
            s = s + "\tU.m"+str(get(v,j,N))+" += U1.m"+str(get(v,k,N))+" * Q.X.m"+str(get(k,j,N))+";\n"
filedata = filedata.replace('$s04', s)

s = ""
for i in range(N*N):
    s = s + "\tU.m"+str(i)+" = Q.X.m"+str(i)+" - (U.m"+str(i)+" / theta);\n"
filedata = filedata.replace('$s05', s)

s = ""
for i in range(N*N):
    s = s + "\tK.m"+str(i)+" = U.m"+str(i)+";\n"
filedata = filedata.replace('$s06', s)

s = ""
for i in range(M):
    s = s + "\tMatrix AQm"+str(i)+" = scal_div(mat_mul(mat_mul(nX,AQm"+str(i)+"),Q.X),theta);\n"
    #s = s + "\tMatrix AQ.m"+str(i)+".rows = "+str(N*N)+";\n"
    #s = s + "\tMatrix AQ.m"+str(i)+".cols = "+str(1)+";\n"
filedata = filedata.replace('$s07', s)

s = ""
for c in range(N*N):
    for b in range(M):
        s = s + "\tQQ.m"+str(get(c,b,N))+" = AQm"+str(b)+".m"+str(get(0,c,N))+";\n"#todo check
filedata = filedata.replace('$s08', s)

s = ""
for e in range(N*N):
    for d in range(M):
        s = s + "\tR.m"+str(get(d,e,N*N))+" = AQm"+str(d)+".m"+str(e)+";\n"
filedata = filedata.replace('$s09', s)

s = ""
for f in range(N*N):
    for g in range(N*N):
        if (f == g):
            s = s + "\tLL.m"+str(get(f,g,(N*N+M)))+" = (fixed_point_precision_16_16)1.0;\n"
    for g in range(N*N, (N*N+M)):
        s = s + "\tLL.m"+str(get(f,g,(N*N+M)))+" = QQ.m"+str(get(f,(g-(N*N)),M))+";\n"
filedata = filedata.replace('$s10', s)

s = ""
for i in range(N*N,(N*N+M)):
    for j in range(N*N):
        if (f == g):
            s = s + "\tLL.m"+str(get(i,j,(N*N+M)))+" = R.m"+str(get(i-(N*N),j,(N*N)))+";\n"
    for h in range(N*N, (N*N+M)):
        s = s + "\tLL.m"+str(get(i,h,(N*N+M)))+" = (fixed_point_precision_16_16)0.0;\n"
filedata = filedata.replace('$s11', s)

s = ""
for i in range(N*N+M):
    s = s + "\te.m"+str(i)+" = K.m"+str(i)+";\n"
filedata = filedata.replace('$s12', s)

s = ""
for l in range(N*N+M):
    for g in range(N*N+M):
        if (l==g):
            s = s + "\tPD.m"+str(get(l,g,(N*N+M)))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$s13', s)

#swap
s = "\tfixed_point_precision_16_16 cmp3; int max_j3;\n"
for y in range((N*N + M)):
    s += "\tmax_j3 = " + str(y)+";\n"
    for r in range(N*N + M):
        for sel in range((N*N + M)):
            "\tif (max_j3 == "+str(sel)+"){\tcmp3 = LL.m("+str(get(sel,y,(N*N + M)))+";}\n"
        s += "\tif (abs_val(LL.m"+str(get(r,y,(N*N + M)))+") > abs_val(cmp3)){ max_j3 = "+str(r)+"; }\n"
    for sel in range(N*N + M):
        s+= "if (max_j3 == "+str(sel)+"){"
        if (sel != y):
            s += "\tfixed_point_precision_16_16 temp3"+str(y)+";\n"
            for sr in range(N*N + M):
                s += "\ttemp3"+str(y)+" = PD.m"+str(get(y,sr,(N*N + M)))+";\n"
                s += "\tPD.m"+str(get(y,sr,(N*N + M)))+" = PD.m"+str(get(sel,sr,(N*N + M)))+";\n"
                s += "\tPD.m"+str(get(sel,sr,(N*N + M)))+" = temp3"+str(y)+";\n"
        s += "}\n"
filedata = filedata.replace('$swap3', s)


s = ""
for v in range(N*N+M):
    for j in range(N*N+M):
        for k in range(N*N+M):
            s = s + "\tLLp.m"+str(get(v,j,(N*N+M)))+" = PD.m"+str(get(v,k,(N*N+M)))+" * LL.m"+str(get(k,j,(N*N+M)))+";\n"
filedata = filedata.replace('$s15', s)

s = ""
for v in range(N*N+M):
    s = s + "\tLD.m"+str(get(v,v,(N*N+M)))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$s16', s)

s = "\tfixed_point_precision_16_16 se;\n"
for i in range(N*N+M):
    for j in range(N*N+M):
        if (i<=j):
            s = s + "\tse = 0;\n"
            for c in range(j):
                s = s + "\tse += LD.m"+str(get(j,c,(N*N+M)))+" * UD.m"+str(get(c,i,(N*N+M)))+";\n"
            s = s + "\tUD.m"+str(get(j,i,(N*N+M)))+" = LLp.m"+str(get(j,i,(N*N+M)))+" - se;\n"
        if (j >= i):
            s = s + "\tse = 0;\n"
            for k in range(i):
                s = s + "\tse += LD.m"+str(get(j,k,(N*N+M)))+" * UD.m"+str(get(k,i,(N*N+M)))+";\n"
            s = s + "\tLD.m"+str(get(j,i,(N*N+M)))+" = (LLp.m"+str(get(j,i,(N*N+M)))+" - se) / UD.m"+str(get(i,i,(N*N+M)))+";\n"
filedata = filedata.replace('$s17', s)

s = ""
for t in range(N*N+M):
    for u in range(N*N+M):
        if (t==u):
            s = s + "\tI.m"+str(get(t,u,(N*N+M)))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$s18', s)

s = ""
for t in range(N*N+M):
    for u in range(N*N+M):
        s = s + "\tLU.m"+str(get(t,u,(N*N+M)))+" = LD.m"+str(get(t,u,(N*N+M)))+" + UD.m"+str(get(t,u,(N*N+M)))+" - I.m"+str(get(t,u,(N*N+M)))+";\n"
filedata = filedata.replace('$s19', s)

s = ""
for i in range(N*N+M):
    for k in range(N*N+M):
        s = s + "\tz.m"+str(i)+" += PD.m"+str(get(i,k,(N*N+M)))+" * e.m"+str(k)+";\n"
filedata = filedata.replace('$s20', s)

s = ""
for a in range(N*N+M-1):
    for b in range(a+1, N*N+M):
        s = s + "\tz.m"+str(b)+" += z.m"+str(b)+" - (LU.m"+str(get(b,a,(N*N+M)))+" * z.m"+str(a)+");\n"
filedata = filedata.replace('$s21', s)

s = ""
for i in range(N*N+M-1, 0, -1):
    s = s + "\tz.m"+str(i)+" = z.m"+str(i)+" / LU.m"+str(get(i,i,(N*N+M)))+";\n"
    for j in range(i):
        s = s + "\tz.m"+str(j)+" += z.m"+str(j)+" - (LU.m"+str(get(j,i,(N*N+M)))+" * z.m"+str(i)+");\n"
filedata = filedata.replace('$s22', s)

s = ""
for l in range(N*N):
    s = s + "\tD.m"+str(l)+" = z.m"+str(l)+";\n"
filedata = filedata.replace('$i1', s)

s = ""
for o in range(N*N, N*N+M):
    s = s + "\tnew_y.m"+str(o-N*N)+" = z.m"+str(o)+";\n"
filedata = filedata.replace('$s23', s)

s = ""
for v in range(N):
    for j in range(N):
        for k in range(N):
            s = s + "\tU10.m"+str(get(v,j,N))+" += Q.X.m"+str(get(v,k,N))+" * C.m"+str(get(k,j,N))+";\n"
filedata = filedata.replace('$s24', s)

s = ""
for v in range(N):
    for j in range(N):
        for k in range(N):
            s = s + "\tU0.m"+str(get(v,j,N))+" += U10.m"+str(get(v,k,N))+" * Q.X.m"+str(get(k,j,N))+";\n"
filedata = filedata.replace('$s25', s)

s = ""
for i in range(N*N):
    s = s + "\tU0.m"+str(i)+" = Q.X.m"+str(i)+" - ( U0.m"+str(i)+" / theta);\n"
filedata = filedata.replace('$s26', s)

s = ""
for i in range(N*N):
    s = s + "\tK0.m"+str(i)+" = U0.m"+str(i)+";\n"
filedata = filedata.replace('$s27', s)

s = ""
for i in range(N*N):
    s = s + "\tnX0.m"+str(i)+" = Q.X.m"+str(i)+"* -1;\n"
filedata = filedata.replace('$s28', s)

s = ""
for m in range(M):
    s = s + "Matrix ATz"+str(m)+" = " + matrix_init;
for v in range(N):
    for j in range(N):
        for k in range(N):
            for m in range(M):
                s = s + "\tATz"+str(m)+".m"+str(get(v,j,N))+" += nX0.m"+str(get(v,k,N))+" * A.m"+str(m)+".m"+str(get(k,j,N))+";\n"
filedata = filedata.replace('$s29', s)

s = ""
for m in range(M):
    s = s + "Matrix ATzz"+str(m)+" = " + matrix_init;
for v in range(N):
    for j in range(N):
        for k in range(N):
            for m in range(M):
                s = s + "\tATzz"+str(m)+".m"+str(get(v,j,N))+" += ATz"+str(m)+".m"+str(get(v,k,N))+" * Q.X.m"+str(get(k,j,N))+";\n"
filedata = filedata.replace('$s30', s)

s = ""
for i in range(N*N):
    for m in range(M):
        s = s + "\tATzz"+str(m)+".m"+str(i)+" = ATzz"+str(m)+".m"+str(i)+" / theta;\n"
filedata = filedata.replace('$s31', s)

s = ""
for c in range(N*N):
    for m in range(M):
        s = s + "\tQQ0.m"+str(get(c,m,M))+" = ATzz"+str(m)+".m"+str(get(0,c,N))+";\n"
filedata = filedata.replace('$s32', s)

s = ""
for e in range(N*N):
    for m in range(M):
        s = s + "\tR0.m"+str(get(m,e,N*N))+" = A.m"+str(m)+".m"+str(e)+";\n"
filedata = filedata.replace('$s33', s)

s = ""
for f in range(N*N):
    for g in range(N*N):
        if (f == g):
            s = s + "\tLL0.m"+str(get(f,g,(N*N+M)))+" = (fixed_point_precision_16_16)1.0;\n"
    for g in range(N*N, (N*N+M)):
        s = s + "\tLL0.m"+str(get(f,g,(N*N+M)))+" = QQ0.m"+str(get(f,(g-(N*N)),M))+";\n"
filedata = filedata.replace('$z13', s)

s = ""
for i in range(N*N,(N*N+M)):
    for j in range(N*N):
        if (f == g):
            s = s + "\tLL0.m"+str(get(i,j,(N*N+M)))+" = R0.m"+str(get(i-(N*N),j,(N*N)))+";\n"
    for h in range(N*N, (N*N+M)):
        s = s + "\tLL0.m"+str(get(i,h,(N*N+M)))+" = (fixed_point_precision_16_16)0.0;\n"
filedata = filedata.replace('$z12', s)


s = ""
for i in range(N*N+M):
    s = s + "\te0.m"+str(i)+" = K0.m"+str(i)+";\n"
filedata = filedata.replace('$z11', s)

s = ""
for l in range(N*N+M):
    for g in range(N*N+M):
        if (l==g):
            s = s + "\tPD0.m"+str(get(l,g,(N*N+M)))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$z10', s)

#swap
s = "\tfixed_point_precision_16_16 cmp0; int max_j0;\n"
for y in range((N*N + M)):
    s += "\tmax_j0 = " + str(y)+";\n"
    for r in range(N*N + M):
        for sel in range((N*N + M)):
            "\tif (max_j0 == "+str(sel)+"){\tcmp0 = LL0.m("+str(get(sel,y,(N*N + M)))+";}\n"
        s += "\tif (abs_val(LL0.m"+str(get(r,y,(N*N + M)))+") > abs_val(cmp0)){ max_j0 = "+str(r)+"; }\n"
    for sel in range(N*N + M):
        s+= "if (max_j0 == "+str(sel)+"){"
        if (sel != y):
            s += "\tfixed_point_precision_16_16 temp0"+str(y)+";\n"
            for sr in range(N*N + M):
                s += "\ttemp0"+str(y)+" = PD0.m"+str(get(y,sr,(N*N + M)))+";\n"
                s += "\tPD0.m"+str(get(y,sr,(N*N + M)))+" = PD0.m"+str(get(sel,sr,(N*N + M)))+";\n"
                s += "\tPD0.m"+str(get(sel,sr,(N*N + M)))+" = temp0"+str(y)+";\n"
        s += "}\n"
filedata = filedata.replace('$swap4', s)


s = ""
for v in range(N*N+M):
    for j in range(N*N+M):
        for k in range(N*N+M):
            s = s + "\tLLp0.m"+str(get(v,j,(N*N+M)))+" = PD0.m"+str(get(v,k,(N*N+M)))+" * LL0.m"+str(get(k,j,(N*N+M)))+";\n"
filedata = filedata.replace('$z09', s)


s = ""
for v in range(N*N+M):
    s = s + "\tLD0.m"+str(get(v,v,(N*N+M)))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$z08', s)

s = "\tfixed_point_precision_16_16 se0;\n"
for i in range(N*N+M):
    for j in range(N*N+M):
        if (i<=j):
            s = s + "\tse0 = 0;\n"
            for c in range(j):
                s = s + "\tse0 += LD0.m"+str(get(j,c,(N*N+M)))+" * UD0.m"+str(get(c,i,(N*N+M)))+";\n"
            s = s + "\tUD0.m"+str(get(j,i,(N*N+M)))+" = LLp0.m"+str(get(j,i,(N*N+M)))+" - se0;\n"
        if (j >= i):
            s = s + "\tse0 = 0;\n"
            for k in range(i):
                s = s + "\tse0 += LD0.m"+str(get(j,k,(N*N+M)))+" * UD0.m"+str(get(k,i,(N*N+M)))+";\n"
            s = s + "\tLD0.m"+str(get(j,i,(N*N+M)))+" = (LLp0.m"+str(get(j,i,(N*N+M)))+" - se0) / UD0.m"+str(get(i,i,(N*N+M)))+";\n"
filedata = filedata.replace('$z07', s)

s = ""
for t in range(N*N+M):
    for u in range(N*N+M):
        s = s + "\tI0.m"+str(get(t,u,(N*N+M)))+" = (fixed_point_precision_16_16)1.0;\n"
filedata = filedata.replace('$zi', s)

s = ""
for t in range(N*N+M):
    for u in range(N*N+M):
        s = s + "\tLU0.m"+str(get(t,u,(N*N+M)))+" = LD0.m"+str(get(t,u,(N*N+M)))+" + UD0.m"+str(get(t,u,(N*N+M)))+" - I0.m"+str(get(t,u,(N*N+M)))+";\n"
filedata = filedata.replace('$z06', s)

s = ""
for i in range(N*N+M):
    for k in range(N*N+M):
        s = s + "\tz0.m"+str(i)+" += PD0.m"+str(get(i,k,(N*N+M)))+" * e0.m"+str(k)+";\n"
filedata = filedata.replace('$z05', s)


s = ""
for a in range(N*N+M-1):
    for b in range(a+1, N*N+M):
        s = s + "\tz0.m"+str(b)+" += z0.m"+str(b)+" - (LU0.m"+str(get(b,a,(N*N+M)))+" * z0.m"+str(a)+");\n"
filedata = filedata.replace('$z04', s)

s = ""
for i in range(N*N+M-1, 0, -1):
    s = s + "\tz0.m"+str(i)+" = z0.m"+str(i)+" / LU0.m"+str(get(i,i,(N*N+M)))+";\n"
    for j in range(i):
        s = s + "\tz0.m"+str(j)+" += z0.m"+str(j)+" - (LU0.m"+str(get(j,i,(N*N+M)))+" * z0.m"+str(i)+");\n"
filedata = filedata.replace('$z03', s)

s = ""
for i in range(N*N):
    s = s + "\tD0.m"+str(i)+" = z0.m"+str(i)+";\n"
filedata = filedata.replace('$z02', s)

s = ""
for i in range(N*N, N*N+M):
    s = s + "\tnew_y.m"+str(i-(N*N))+" = z0.m"+str(i)+";\n"
filedata = filedata.replace('$z01', s)


# CHECK

s = ""
for i in range(M):
    s = s + "\tsolved = solved && d_equal(dot(A.m"+str(i)+", X),b.m"+str(i)+");\n"
filedata = filedata.replace('$check2', s)

s = ""
for i in range(N):
    s += "\tS.m"+str(i)+" = C.m"+str(i)+";\n"
    for m in range(M):
        s = s + "\tS.m"+str(i)+" = S.m"+str(i)+" - (A.m"+str(m)+".m"+str(i)+" * y.m"+str(m)+");\n"
filedata = filedata.replace('$check3', s)


s = ""
for i in range(M):
    s = s + "\tif (!(d_equal(dot(A.m"+str(i)+", X),b.m"+str(i)+"))){ solved = 1; }\n"
filedata = filedata.replace('$check1', s)

s = ""
for i in range(N):
    for j in range(N):
        s = s + '\tprintf(" %6.3f", X.m'+str(get(i,j,N))+');\n'
    s += 'printf("\\n");'
filedata = filedata.replace('$print_mat', s)

s = ""
for i in range(BN):
    for j in range(BN):
        s = s + '\tprintf(" %6.3f", X.m'+str(get(i,j,BN))+');\n'
    s += 'printf("\\n");'
filedata = filedata.replace('$print_b_mat', s)

s = ""
for i in range(M):
    s = s + '\tprintf(" %6.3f", v.m'+str(i)+');\n'
    s += 'printf("\\n");'
filedata = filedata.replace('$print_vec', s)

s = ""
for i in range(BM):
    s = s + '\tprintf(" %6.3f", v.m'+str(i)+');\n'
    s += 'printf("\\n");'
filedata = filedata.replace('$print_b_vec', s)


# Write the file out again
with open('sdp_new.c', 'w') as file:
  file.write(filedata)
