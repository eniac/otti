//specify problem size here
#define C 3 //constraints
#define V 2 //vars
#define CP 4 //constraints
#define VP 3 //vars

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
  _Bool stars[VP]; // V+1
  int cntr;
} Tableau;

typedef struct {
  fixed_point_precision_16_16 vars[V]; // V
  int lt_gt;
  fixed_point_precision_16_16 goal;
} Constraint;

typedef struct {
  fixed_point_precision_16_16 x[VP]; //V+1
  fixed_point_precision_16_16 y[CP]; //C+1
} Solution;

typedef struct {

  fixed_point_precision_16_16 m0;
  fixed_point_precision_16_16 m1;
  fixed_point_precision_16_16 m2;
  fixed_point_precision_16_16 m3;
  fixed_point_precision_16_16 m4;
  fixed_point_precision_16_16 m5;
  fixed_point_precision_16_16 m6;
  fixed_point_precision_16_16 m7;
  fixed_point_precision_16_16 m8;
  fixed_point_precision_16_16 m9;
  fixed_point_precision_16_16 m10;
  fixed_point_precision_16_16 m11;
  fixed_point_precision_16_16 m12;
  fixed_point_precision_16_16 m13;
  fixed_point_precision_16_16 m14;
  fixed_point_precision_16_16 m15;
  fixed_point_precision_16_16 m16;
  fixed_point_precision_16_16 m17;
  fixed_point_precision_16_16 m18;
  fixed_point_precision_16_16 m19;
  fixed_point_precision_16_16 m20;
  fixed_point_precision_16_16 m21;
  fixed_point_precision_16_16 m22;
  fixed_point_precision_16_16 m23;
  fixed_point_precision_16_16 m24;
  fixed_point_precision_16_16 m25;
  fixed_point_precision_16_16 m26;
  fixed_point_precision_16_16 m27;
  fixed_point_precision_16_16 m28;
  fixed_point_precision_16_16 m29;


  fixed_point_precision_16_16 x0;
  fixed_point_precision_16_16 x1;
  fixed_point_precision_16_16 x2;

  fixed_point_precision_16_16 y0;
  fixed_point_precision_16_16 y1;
  fixed_point_precision_16_16 y2;
  fixed_point_precision_16_16 y3;


} Static_Fix;

//function declr
int d_equal(fixed_point_precision_16_16 a, fixed_point_precision_16_16 b);
fixed_point_precision_16_16 get(int i, int j, Tableau tab);
void print_tab(Tableau tab);
Tableau add_slack(Tableau tableau, int max_min, int cols);
Tableau calculate_dual(Tableau p, int max_min);
Tableau simplex_max(Tableau tableau);
Tableau simplex_stars(Tableau tableau);
fixed_point_precision_16_16 find_opt_var(Tableau t, int j);
Static_Fix simplex_gadget(Tableau p_tableau, int p_max_min);
Tableau make_problem();
Tableau maximize(Tableau t, Constraint c);
Tableau minimize(Tableau t, Constraint c);
Tableau add(Tableau t, Constraint c);
void print_sol(Solution s);
int simplex_check(int max_min, Static_Fix sol);
Static_Fix simplex_prover(Tableau p_tableau, int p_max_min);
