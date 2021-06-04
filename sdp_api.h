#define M 2
#define N 2

//typedef float double;

static const double epsilon   = (double)1.0e-5;
static int valid = 1;

typedef struct {
  int len;
  double v[9];
} Vector;

typedef struct {
  int len;
  double v[500];
} Big_Vector;

typedef struct {
  int len;
  double v[9];
  int sols;
} Gauss_Vec;

typedef struct {
  int rows; int cols;
  double m[9];
} Matrix;

typedef struct {
  int rows; int cols;
  double m[500];
} Big_Matrix;

typedef struct {
  int len;
  Matrix m0;
  Matrix m1;
} Matrix_List;

typedef struct {
  int len;
  Big_Matrix m0;
  Big_Matrix m1;
} Big_Matrix_List;

typedef struct {
  double err;
  Matrix D;
  Vector y;
} Eq_Sol;

typedef struct {
  int feasible;
  Matrix X;
  Vector y;
} Solution;

double test(double a);

double *sdp_solve(int n, int m, double *c, double *x, double *a, double *b);
double get(int i, int j, Matrix mat);
int d_equal(double a, double b);
int d_equal_ep(double a, double b,double ep);
void print_mat(Matrix X);
void print_mat_b(Big_Matrix X);
void print_vec(Vector v);
void print_vec_b(Big_Vector v);
double abs_val(double x);
double sqrt_val(double num);
double dot(Matrix A, Matrix B);
double vec_comb(Vector a, Vector b);
Matrix scal_div(Matrix A, double s);
Matrix mat_mul(Matrix A, Matrix B);
Vector vec_mul(Matrix A, Vector b);
Big_Vector vec_mul_big(Big_Matrix A, Big_Vector b);
double vec_vec_mul(Vector a, Vector b);
Matrix scal_mul(Matrix A, double s);
Matrix mat_add(Matrix A, Matrix B);
Matrix mat_sub(Matrix A, Matrix B);
Matrix mat_comb(Vector y, Matrix_List A);
Matrix transpose(Matrix A);
Vector vec_sub(Vector a, Vector b);
Big_Vector vec_sub_big(Big_Vector a, Big_Vector b);
Big_Vector vectorize(Big_Matrix A);
Big_Matrix biggify_mat(Matrix P, int rows, int cols);
Vector reg_vec(Gauss_Vec g);
Matrix swap_rows(Matrix X, int a, int b);
Big_Matrix swap_rows_big(Big_Matrix X, int a, int b);
Matrix div_row(Matrix X, int a, double s);
Matrix flatten(Matrix X);
Matrix sub_multd_row(Matrix X, int a, int b, double s);
double norm_vec_big(Big_Vector v);
double norm_mat(Matrix X);
Matrix_List LUP_decompose(Matrix A);
Big_Matrix_List LUP_decompose_big(Big_Matrix A);
Big_Vector LUP_solve(Big_Matrix A, Big_Vector b);
Eq_Sol solve_eq(Matrix X, Matrix_List A, Matrix C, double theta);
Matrix cholesky(Matrix X);
int psd(Matrix X);
Matrix inverse(Matrix X);
Matrix beta_approx(Matrix C, Matrix Xp, Matrix_List A, Vector b, double theta, double beta);
Matrix matrixize(Vector a, int rows, int cols);
Solution sdp(Matrix C, Matrix Xp, Matrix_List A, Vector b, double theta, double beta);
int sdp_check(Matrix C, Matrix X, Matrix_List A, Vector b, Vector y, int feasible);
Matrix sdp_gadget(Matrix C, Matrix X, Matrix_List A, Vector b);
double get_big(int i, int j, Big_Matrix mat);
double norm_circ(Big_Vector e, Big_Matrix LL, Big_Vector z);
