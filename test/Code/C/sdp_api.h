#define M 2
#define N 2

typedef float fixed_point_precision_16_16;

static const fixed_point_precision_16_16 epsilon   = (fixed_point_precision_16_16)1.0e-2;
static int valid = 1;

typedef struct {
  int len;
  fixed_point_precision_16_16 v[4];
} Vector;

typedef struct {
  int len;
  fixed_point_precision_16_16 v[10];
} Big_Vector;

typedef struct {
  int len;
  fixed_point_precision_16_16 v[4];
  int sols;
} Gauss_Vec;

typedef struct {
  int rows; int cols;
  fixed_point_precision_16_16 m[4];
} Matrix;

typedef struct {
  int rows; int cols;
  fixed_point_precision_16_16 m[10];
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
  fixed_point_precision_16_16 err;
  Matrix D;
  Vector y;
} Eq_Sol;

typedef struct {
  int feasible;
  Matrix X;
  Vector y;
} Solution;

typedef struct {
  fixed_point_precision_16_16 A0_0;
  fixed_point_precision_16_16 A0_1;
  fixed_point_precision_16_16 A0_2;
  fixed_point_precision_16_16 A0_3;

  fixed_point_precision_16_16 A1_0;
  fixed_point_precision_16_16 A1_1;
  fixed_point_precision_16_16 A1_2;
  fixed_point_precision_16_16 A1_3;

  fixed_point_precision_16_16 C_0;
  fixed_point_precision_16_16 C_1;
  fixed_point_precision_16_16 C_2;
  fixed_point_precision_16_16 C_3;



  fixed_point_precision_16_16 X_0;
  fixed_point_precision_16_16 X_1;
  fixed_point_precision_16_16 X_2;
  fixed_point_precision_16_16 X_3;

  fixed_point_precision_16_16 b0;
  fixed_point_precision_16_16 b1;

  fixed_point_precision_16_16 y0;
  fixed_point_precision_16_16 y1;

  int feasible;

} Problem_Box;

fixed_point_precision_16_16 get(int i, int j, Matrix mat);
int d_equal(fixed_point_precision_16_16 a, fixed_point_precision_16_16 b);
int d_equal_ep(fixed_point_precision_16_16 a, fixed_point_precision_16_16 b,fixed_point_precision_16_16 ep);
void print_mat(Matrix X);
void print_mat_b(Big_Matrix X);
void print_vec(Vector v);
void print_vec_b(Big_Vector v);
fixed_point_precision_16_16 abs_val(fixed_point_precision_16_16 x);
fixed_point_precision_16_16 sqrt_val(fixed_point_precision_16_16 num);
fixed_point_precision_16_16 dot(Matrix A, Matrix B, int size);
fixed_point_precision_16_16 vec_comb(Vector a, Vector b);
Matrix scal_div(Matrix A, fixed_point_precision_16_16 s);
Matrix mat_mul(Matrix A, Matrix B);
Vector vec_mul(Matrix A, Vector b);
Big_Vector vec_mul_big(Big_Matrix A, Big_Vector b);
fixed_point_precision_16_16 vec_vec_mul(Vector a, Vector b);
Matrix scal_mul(Matrix A, fixed_point_precision_16_16 s);
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
Matrix div_row(Matrix X, int a, fixed_point_precision_16_16 s);
Matrix flatten(Matrix X);
Matrix sub_multd_row(Matrix X, int a, int b, fixed_point_precision_16_16 s);
fixed_point_precision_16_16 norm_vec_big(Big_Vector v);
fixed_point_precision_16_16 norm_mat(Matrix X);
Matrix_List LUP_decompose(Matrix A);
Big_Matrix_List LUP_decompose_big(Big_Matrix A);
Big_Vector LUP_solve(Big_Matrix A, Big_Vector b);
Eq_Sol solve_eq(Matrix X, Matrix_List A, Matrix C, fixed_point_precision_16_16 theta);
Matrix cholesky(Matrix X);
int psd(Matrix X);
Matrix inverse(Matrix X);
Matrix beta_approx(Matrix C, Matrix Xp, Matrix_List A, Vector b, fixed_point_precision_16_16 theta, fixed_point_precision_16_16 beta);
Matrix matrixize(Vector a, int rows, int cols);
Problem_Box sdp(Matrix C, Matrix Xp, Matrix_List A, Vector b, fixed_point_precision_16_16 theta, fixed_point_precision_16_16 beta);
int sdp_check(Problem_Box P, int feasible);
Matrix sdp_gadget(Matrix C, Matrix X, Matrix_List A, Vector b);
fixed_point_precision_16_16 get_big(int i, int j, Big_Matrix mat, int c);
fixed_point_precision_16_16 norm_circ(Big_Vector e, Big_Matrix LL, int size, Big_Vector z);
fixed_point_precision_16_16 pow_2(fixed_point_precision_16_16 b);
