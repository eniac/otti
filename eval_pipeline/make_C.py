import pickle
import sys
import numpy as np

filename = sys.argv[1]
prob = dict();

# get tableau
with open(filename, 'rb') as handle:
  prob = pickle.load(handle)

min_max = prob['min_max'] # 'min' or 'max'
tableau = prob['tableau'] # np.mat

rows, cols = tableau.shape
var = cols - 1
eq = rows - 1
V = str(var)
C = str(eq)
VP = str(var+1)
CP = str(eq+1)
UB = (VP+C)*CP #upper bound accounts for slack
LB = CP * VP;

probname = filename.replace('.pickle','')

# write .h file
hfilename = "LP_" + C + "_" + V + "_" + probname + ".h"
hfile = open(hfilename, "w+")

# boilerplate
w = "#define C "+C+"\n#define V "+V+"\n#define CP "+CP+"\n#define VP "+VP+"\n#define LEQ 0\n#define GEQ 1\n#define MAX 1\n#define MIN 0\ntypedef float fixed_point_precision_16_16;\n"

# write size specific structures
w = w + "typedef struct {\n  int rows, cols;\n  fixed_point_precision_16_16 mat["+str(UB)+"];\n  _Bool stars[VP];\n  int cntr;\n} Tableau;\n\ntypedef struct {\n"

for i in range(0,UB):
  w = w + "fixed_point_precision_16_16 m"+str(i)+";\n"
w = w + "\n"

for i in range(0,VP):
  w = w + "fixed_point_precision_16_16 x"+str(i)+";\n"
w = w + "\n"

for i in range(0,CP):
  w = w + "fixed_point_precision_16_16 y"+str(i)+";\n"
w = w + "\n} Solution_Box;\n\n"

hfile.write(w)

# copy template
htemplate = open("LP_template.h", "r")
for line in htemplate:
  hfile.write(line)
htemplate.close()


hfile.close()



# write .c file
cfilename = "LP_" + str(eq) + "_" + str(var) + "_" + probname  + ".c"
cfile = open(cfilename, "w+")

# beginning boilerplate
cfile.write('#include <stdio.h>\n#include "'+hfilename+'"\n\nint main(void) {\n')

# main function with tableau
obj = "Constraint obj = {{")
for i in range(1,VP):
  obj = obj + str(float(tableau[0][i]))
  if (i < VP-1): obj = obj + ", "

obj = obj + "}, 0, 0};\nTableau tab ="

if min_max = "max":
  obj = obj + "maximize(obj);\n"
else:
  obj = obj + "minimize(obj);\n"

for j in range(1,CP):
  obj = obj + "Constraint c" + str(j) + " = {{"
  for i in range(1,VP):
      obj = obj + str(float(tableau[j][i]))
      if (i < VP-1): obj = obj + ", "

  obj = obj + "}, GEQ, " + str(float(tableau[j][0])) + "};\ntab = add(tab, c" + str(j) + ");\n"

obj = obj + "Solution_Box solution = simplex_gadget(tab, "
if min_max = "max":
  obj = obj + "MAX);\n"
else:
  obj = obj + "MIN);\n"

obj = obj + "return 0;\n}\n"

cfile.write(obj)

# make problem
m = "Tableau make_problem(){\n  Tableau tab = { CP, V+C+1, {"
for i in range(UB):
  m = m+"0"
  if (i<UB-1): m = m+","
m = m+ "}, {"
for i in range(VP):
  m = m+"0"
  if (i<VP-1): m = m+","
m = m+"}, 1};\n  return tab;\n}"
cfile.write(m)

# sdp_prove specifics
p = "Solution_Box simplex_prover(Tableau p_tableau, int p_max_min) {\n  Tableau p_sol_tab = add_slack(p_tableau, p_max_min, V);\n  Tableau p_sol_tab_b = simplex_stars(p_sol_tab_a);\n  Tableau p_sol_tab = simplex_max(p_sol_tab_b);\n  int d_max_min = !p_max_min;\n  Tableau d_tableau = calculate_dual(p_tableau, d_max_min);\n  Tableau d_sol_tab = add_slack(d_tableau, d_max_min, C);\n  Tableau d_sol_tab_b = simplex_stars(d_sol_tab_a);\n  Tableau d_sol_tab = simplex_max(d_sol_tab_b);\n  Solution sol = {{0,0,0,0,0,0}, {0,0,0,0,0,0}};\n"

p = p + "  for(int i=0; i<V; i++) {\n    sol.x[i] = find_opt_var(p_sol_tab, (i+1));\n  }\n  sol.x[V] = p_sol_tab.mat[0];\n  fixed_point_precision_16_16 y[CP];\n  for(int j=0; j<C; j++) {\n    sol.y[j] = find_opt_var(d_sol_tab, (j+1));\n  }\n  sol.y[C] = d_sol_tab.mat[0];\n  Solution_Box solf = { "

for i in range(0,UB):
  p = p + "  p_tableau.mat["+str(i)+"]"
  if (i < UB-1): p = p+",\n"
p = p+"\n"

for i in range(0,VP):
  p = p + "sol.x["+str(i)+"]"
  if (i < VP-1): p = p+",\n"
p = p+"\n"

for i in range(0,CP):
  p = p + "sol.y["+str(i)+"]"
  if (i < CP-1): p = p+",\n"
p = p+"\n  };\n  return solf;\n\n}\n\n"
cfile.write(p)

# write check specifics
k = "int simplex_check(int max_min, Solution_Box sf) {\nint sol_eq = 0;\n"

k = k + "fixed_point_precision_16_16 xc = "  
for i in range(0,V):
  k = k + "(sf.m"+str(i+1)+" * sf.x" +str(i)+")"
  if (i < V-1): k = k + " + "
k = k + ";\n"
    
k = k + "fixed_point_precision_16_16 yb = "  
for i in range(0,V):
  k = k + "(sf.m"+str((VP+C)*i)+" * sf.y" +str(i)+")"
  if (i < V-1): k = k + " + "
k = k + ";\n"
    
k = k + "if (max_min){ sol_eq = (xc + yb == 0); } else { sol_eq = (xc == yb); }\nint sat = 1;\n"

'''
for a in range(0,VP):
  k = k + "yprod"+str(a)+" = 0;\n"
  for b in range (0, C):
    k = k + "yprod"+str(a)+" += (sf.m"+str(a*(VP+C)+b+1)+" * sf.y"+str(b)+")""
k = k + ";\n"
'''

for a in range(0,CP):
  k = k + "prod"+str(a)+" = 0;\n"
  for b in range (0, V):
    k = k + "prod"+str(a)+" += (sf.m"+str(a*(VP+C)+b+1)+" * sf.x"+str(b)+")""

k = k + ";\nif (max_min){\n  sat = \n"

for i in range(O,CP):
  k = k + "(d_equal(sf.m"+str((VP+C)*i)+",prod"+str(i)+") || sf.m"+str((VP+C)*i)+" > prod"+str(i)+")"
  if (i < CP-1): k = k + " && "

'''
#NEW Y TODO
k = k + ";\nsat = sat && "
for i in range(O,CP):
  k = k + "(d_equal(sf.m"+str((VP+C)*i)+",yprod"+str(i)+") || sf.m"+str((VP+C)*i)+" < prod"+str(i)+")"
  if (i < CP-1): k = k + " && "
'''

k = k + ";\n} else {  sat = \n"


for i in range(O,CP):
  k = k + "(d_equal(sf.m"+str((VP+C)*i)+",prod"+str(i)+") || sf.m"+str((VP+C)*i)+" < prod"+str(i)+")"
  if (i < CP-1): k = k + " && "

'''
#TODO CHANGE
k = k + ";\nsat = sat && "
for i in range(O,CP):
  k = k + "(d_equal(sf.m"+str((VP+C)*i)+",yprod"+str(i)+") || sf.m"+str((VP+C)*i)+" > prod"+str(i)+")"
  if (i < CP-1): k = k + " && "
'''
k = k + ";\n}\nreturn sol_eq && sat;\n}"

cfile.write(k)

# calc dual
d = "Tableau calculate_dual(Tableau p, int max_min){\n  Tableau transpose = {VP, C+V+1, {"
for i in range(UB):
  d = d+"0"
  if (i<UB-1): d = d+","
d = d+ "}, {"
for i in range(VP):
  d = d+"0"
  if (i<VP-1): d = d+","
d = d+"}, 1};\n"
cfile.write(d)

# note incomplete - template complete function
# copy template
ctemplate = open("LP_template.c", "r")
for line in ctemplate:
  cfile.write(line)
ctemplate.close()


cfile.close()



