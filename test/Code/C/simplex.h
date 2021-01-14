//specify problem size here
#define C 3 //constraints
#define V 2 //vars

//compiler-friendly structs/types
#define LEQ 0
#define GEQ 1
#define MAX 1
#define MIN 0

typedef float fixed_point_precision_16_16;
static const fixed_point_precision_16_16 epsilon   = (fixed_point_precision_16_16)1.0e-10;

typedef struct {
  int rows, cols; // mat[m x n]
  fixed_point_precision_16_16 mat[30]; //upper bound yourself
  _Bool stars[V+1]; // V+1
  int cntr;
} Tableau;

typedef struct {
  fixed_point_precision_16_16 vars[V]; // V
  int lt_gt;
  fixed_point_precision_16_16 goal;
} Constraint;

typedef struct {
  fixed_point_precision_16_16 x[V+1]; //V+1
  fixed_point_precision_16_16 y[C+1]; //C+1
} Solution;

//function declr
int d_equal(fixed_point_precision_16_16 a, fixed_point_precision_16_16 b);
fixed_point_precision_16_16 get(int i, int j, Tableau tab);
void print_tab(Tableau tab);
Tableau add_slack(Tableau tableau, int max_min, int cols);
Tableau calculate_dual(Tableau p, int max_min);
Tableau simplex_max(Tableau tableau);
Tableau simplex_stars(Tableau tableau);
int satisfies(fixed_point_precision_16_16 sol[], int sol_len, Tableau tab, int max_min);
fixed_point_precision_16_16 find_opt_var(Tableau t, int j);
int solution_eq(fixed_point_precision_16_16 c[], fixed_point_precision_16_16 x[], int lenx, fixed_point_precision_16_16 y[], fixed_point_precision_16_16 b[], int leny, int max_min);
Solution simplex_gadget(Tableau p_tableau, int p_max_min);
Tableau make_problem();
Tableau maximize(Tableau t, Constraint c);
Tableau minimize(Tableau t, Constraint c);
Tableau add(Tableau t, Constraint c);
void print_sol(Solution s);
int simplex_check(fixed_point_precision_16_16 c[], fixed_point_precision_16_16 x[], int lenx, fixed_point_precision_16_16 y[], fixed_point_precision_16_16 b[], int leny, int max_min, Tableau p_tableau);
Solution simplex_prover(Tableau p_tableau, int p_max_min);
