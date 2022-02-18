from lpsolve55 import *
import sys
lp = lpsolve('read_mps', sys.argv[2])
lpsolve('set_verbose', lp, IMPORTANT)
lpsolve('solve',lp)
n_constraints = lpsolve('get_Norig_rows', lp)
if (sys.argv[1] == "--dual"):
    [duals, ret] = lpsolve('get_dual_solution', lp)
    print duals[:n_constraints]
else:
    [primals, ret] = lpsolve('get_primal_solution', lp)
    print primals[(n_constraints+1):]

# dlp.  lpsolve('copy_lp',lp)
# lpsolve('dualize_lp',dlp)
# lpsolve('write_lp',dlp,'My_DLp.lp')
