
#define C 5 //constraints
#define V 5 //vars
#define CP 6 //constraints
#define VP 6 //vars

//compiler-friendly structs/types
#define LEQ 0
#define GEQ 1
#define MAX 1
#define MIN 0

typedef float fixed_point_precision_16_16;
static const fixed_point_precision_16_16 epsilon   = (fixed_point_precision_16_16)1.0e-4;

typedef struct {
  int rows, cols; // mat[m x n]
  fixed_point_precision_16_16 mat[70]; //upper bound yourself
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
  fixed_point_precision_16_16 m30;
  fixed_point_precision_16_16 m31;
  fixed_point_precision_16_16 m32;
  fixed_point_precision_16_16 m33;
  fixed_point_precision_16_16 m34;
  fixed_point_precision_16_16 m35;
  fixed_point_precision_16_16 m36;
  fixed_point_precision_16_16 m37;
  fixed_point_precision_16_16 m38;
  fixed_point_precision_16_16 m39;
  fixed_point_precision_16_16 m40;
  fixed_point_precision_16_16 m41;
  fixed_point_precision_16_16 m42;
  fixed_point_precision_16_16 m43;
  fixed_point_precision_16_16 m44;
  fixed_point_precision_16_16 m45;
  fixed_point_precision_16_16 m46;
  fixed_point_precision_16_16 m47;
  fixed_point_precision_16_16 m48;
  fixed_point_precision_16_16 m49;
  fixed_point_precision_16_16 m50;
  fixed_point_precision_16_16 m51;
  fixed_point_precision_16_16 m52;
  fixed_point_precision_16_16 m53;
  fixed_point_precision_16_16 m54;
  fixed_point_precision_16_16 m55;
  fixed_point_precision_16_16 m56;
  fixed_point_precision_16_16 m57;
  fixed_point_precision_16_16 m58;
  fixed_point_precision_16_16 m59;
  fixed_point_precision_16_16 m60;
  fixed_point_precision_16_16 m61;
  fixed_point_precision_16_16 m62;
  fixed_point_precision_16_16 m63;
  fixed_point_precision_16_16 m64;
  fixed_point_precision_16_16 m65;
  fixed_point_precision_16_16 m66;
  fixed_point_precision_16_16 m67;
  fixed_point_precision_16_16 m68;
  fixed_point_precision_16_16 m69;
  fixed_point_precision_16_16 m70;

  fixed_point_precision_16_16 x0;
  fixed_point_precision_16_16 x1;
  fixed_point_precision_16_16 x2;
  fixed_point_precision_16_16 x3;
  fixed_point_precision_16_16 x4;
  fixed_point_precision_16_16 x5;

  fixed_point_precision_16_16 y0;
  fixed_point_precision_16_16 y1;
  fixed_point_precision_16_16 y2;
  fixed_point_precision_16_16 y3;
  fixed_point_precision_16_16 y4;
  fixed_point_precision_16_16 y5;



} Solution_Box;

//function declr
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
