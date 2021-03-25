
static const fixed_point_precision_16_16 epsilon  = (fixed_point_precision_16_16)1.0e-10;

typedef struct {
  fixed_point_precision_16_16 vars[V]; // V
  int lt_gt;
  fixed_point_precision_16_16 goal;
} Constraint;

typedef struct {
  fixed_point_precision_16_16 x[VP]; //V+1
  fixed_point_precision_16_16 y[CP]; //C+1
} Solution;

int d_equal(fixed_point_precision_16_16 a, fixed_point_precision_16_16 b);
fixed_point_precision_16_16 get(int i, int j, Tableau tab);
void print_tab(Tableau tab);
Tableau add_slack(Tableau tableau, int max_min, int cols);
Tableau calculate_dual(Tableau p, int max_min);
Tableau simplex_max(Tableau tableau);
Tableau simplex_stars(Tableau tableau);
fixed_point_precision_16_16 find_opt_var(Tableau t, int j);
Solution_Box simplex_gadget(Tableau p_tableau, int p_max_min);
Tableau make_problem();
Tableau maximize(Constraint c);
Tableau minimize(Constraint c);
Tableau add(Tableau, Constraint c);
void print_sol(Solution s);
int simplex_check(int max_min, Solution_Box sol);
Solution_Box simplex_prover(Tableau p_tableau, int p_max_min);
