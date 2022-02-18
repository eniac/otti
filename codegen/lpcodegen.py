from pysmps import smps_loader as smps
import sys
import traceback
import itertools

var = 'X'
delta = 0.01

class Show:
    def show():
        raise ValueError("Not implemented")
    def __repr__(self):
        return self.show()
    def __str__(self):
        return self.show()

class Sign(Show):
    def __init__(self, s):
        sign = 0
        if(s == 'G'):
            sign = 1
        elif(s == 'E'):
            sign = 0
        elif(s == 'L'):
            sign = -1
        else:
            raise ValueError("Interpret sign")
        self.sign = sign

    def flip(self):
        s = self.sign
        if(s == 1):
            return Sign('L')
        elif(s == 0):
            return Sign('E')
        elif(s == -1):
            return Sign('G')
        else:
            raise ValueError("Flip sign")

    def bound(self):
        if self.sign == 0:
            return "FX"
        elif self.sign == 1:
            return "LO"
        elif self.sign == -1:
            return 'UP'
        else:
            raise ValueError("Wrong sign")

    def show(self):
        if self.sign == 0:
            return "=="
        elif self.sign == 1:
            return ">="
        elif self.sign == -1:
            return "<="
        else:
            raise ValueError("Wrong sign in show")

class Constraint(Show):
    def __init__(self, mult, sign, const):
        self.mult = mult
        self.sign = sign
        self.const = const

    def show(self):
        num_vars = len(self.mult)
        terms = [ None  if m == 0.000 else "{}{}".format(var,v) if m == 1.0 else "{:.8f}*{}{}".format(m, var,v) for (m,v) in zip(self.mult, range(num_vars)) ]
        terms = filter(lambda x: x != None, terms)
        return " + ".join(terms) + " " + self.sign.show() + " {:.8f}".format(self.const)

    def is_zero(self):
        for v in self.mult:
            if v != 0.0:
                return False
        return True

    def show_delta(self):
        num_vars = len(self.mult)
        terms = [ None  if m == 0.000 else "{}{}".format(var,v) if m == 1.0 else "{:.8f}*{}{}".format(m, var,v) for (m,v) in zip(self.mult, range(num_vars)) ]
        terms = filter(lambda x: x != None, terms)
        if self.sign.sign == 0:
            return "deq(" + ", ".join([" + ".join(terms), "{:.8f}".format(self.const), str(delta)]) + ")"
        elif self.sign.sign == 1:
            return "dge(" + ", ".join([" + ".join(terms), "{:.8f}".format(self.const), str(delta)]) + ")"
        elif self.sign.sign == -1:
            return "dle(" + ", ".join([" + ".join(terms), "{:.8f}".format(self.const), str(delta)]) + ")"
        else:
            raise ValueError("Wrong sign in show_delta " + self.show())

# Three types of range constraints x >= 0, x <= 0, x \in R (unbounded)
class RangeConstraint(Show):
    def __init__(self, num_var, b, v):
        self.index = num_var
        if v == float('inf') or v == float('-inf'):
            self.bounded = False
        elif b == 'UP' and v <= 0.0:
            self.sign = Sign('L')
            self.bounded = True
        elif b == 'LO' and v >= 0.0:
            self.sign = Sign('G')
            self.bounded = True
        elif b == 'MI':
            self.sign = Sign('L')
            self.bounded = True
        elif b == 'PL':
            self.sign = Sign('G')
            self.bounded = True
        else:
            self.bounded = False

    def sign_flip(self):
        if self.bounded:
            return self.sign
        else:
            return Sign('E')

    def show(self):
        if(self.bounded):
            return "{}{} {} {:.8f}".format(var,self.index, self.sign.show(), 0.0)
        else:
            return ""

    def show_delta(self):
        if self.sign.sign == 0:
            return "deq(" + "{}{}".format(var, self.index) + ", 0.0, {})".format(delta)
        elif self.sign.sign == 1:
            return "dge(" + "{}{}".format(var, self.index) + ", 0.0, {})".format(delta)
        elif self.sign.sign == -1:
            return "dle(" + "{}{}".format(var, self.index) + ", 0.0, {})".format(delta)
        else:
            raise ValueError("Wrong sign in show_delta " + self.show())

class ObjectiveConstraint(Show):
    def __init__(self, mult):
        self.mult = mult

    def show(self):
        terms = [ None  if m == 0.000 else "{}{}".format(var, i) if m == 1.0 else "{:.8f}*{}{}".format(m, var, i) for (i, m) in enumerate(self.mult) ]
        terms = list(filter(lambda x: x != None, terms))
        if(len(terms) == 0):
            return "0*{}0".format(var)
        return " + ".join(terms)


def ccheckgen(constraints, range_constraints):
    return ",\n\t".join(
            [c.show_delta() for c in constraints if not c.is_zero()] +
            [c.show_delta() for c in range_constraints if c.show() != ""]
        )

# substitution dict
def cvargen(var, num_vars):
    C = """
    fp64 $vars;
    """

    substitutions = {
        'vars' : ", ".join(["{}{} = __GADGET_exist()".format(var,i) for i in range(num_vars)]),
    }

    for (term, subst) in sorted(substitutions.items(), key=lambda k: -len(k[0])):
        C = C.replace("${}".format(term), subst)
    return C

def parse(filename):
    ex = smps.load_mps(filename)
    def rhs():
        if ex[8] == []:
            while True:
                yield 0.0
        else:
            for c in ex[9][ex[8][0]]:
                yield c
            while True:
                yield 0.0

    # Is it max or min objective?
    min_max = 1
    num_vars = len(ex[3])
    pobj = ObjectiveConstraint(ex[6])
    r = list(itertools.islice(rhs(), len(ex[5])))
    dobj = ObjectiveConstraint(r)
    pconstraints = []
    dconstraints = []
    assert(len(ex[7]) == len(ex[5]))
    for (mult, s, ri) in zip(ex[7], ex[5], r):
        pconstraints += [Constraint(mult, Sign(s), ri)]

    # Keeps the ranges of each variable
    bounds = dict([(i, RangeConstraint(i, 'UNCONSTRAINED', 0.0)) for i in range(num_vars)])
    if ex[10] != []:
        bound = ex[10][0]
        for b in ex[11][bound].keys():
            for i, v in enumerate(ex[11][bound][b]):
                if not bounds[i].bounded:
                    bounds[i] = RangeConstraint(i, b, v)

    prangeconstraints = bounds.values() # [v if v.bounded else RangeConstraint(v.index, 'LO', 0.0) for v in bounds.values()]

    for (mult, s, r) in zip(ex[7].transpose(), [c.sign_flip() for c in prangeconstraints], ex[6].transpose()):
        dconstraints += [ Constraint(mult, s, r) ]

    drangeconstraints = [ RangeConstraint(i, c.sign.flip().bound(), 0.0) for (i, c) in enumerate(pconstraints) ]
    return (
        pconstraints, pobj, num_vars, min_max, prangeconstraints,
        dconstraints, dobj, len(pconstraints), 1 - min_max, drangeconstraints
    )

# MAIN
(pconstraints, pobj, pnum_vars, pmin_max, prangeconstraints,
dconstraints, dobj, dnum_vars, dmin_max, drangeconstraints) = parse(sys.argv[1])

c_header = '''
typedef double fp64;

int deq(fp64 a, fp64 b, fp64 delta) {
    return ((-1 * delta) <= (a - b)) && ((a - b) <= delta);
}

int dge(fp64 a, fp64 b, fp64 delta) {
    return (a + delta) >= b;
}

int dle(fp64 a, fp64 b, fp64 delta) {
    return a <= (b + delta);
}

int main() {
'''

mps_call = '    __GADGET_lpsolve("{}");'.format(sys.argv[1])
check_header = "    int check = __GADGET_check("

var = 'X'
primal_obj = pobj.show()
primal_check = ccheckgen(
    pconstraints,
    prangeconstraints
)

var = 'Y'
dual_obj = dobj.show()

certificate = "deq({}, {}, {})".format(
    primal_obj,
    dual_obj,
    delta
);

c_foot = '''    );

    return check;
}
'''

print("\n".join([c_header, cvargen('X', pnum_vars), cvargen('Y', dnum_vars), mps_call, check_header, "\t" + primal_check + ",\n\t" + certificate, c_foot]))

