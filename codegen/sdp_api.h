
static const double epsilon   = (double)1.0e-2;
static int valid = 1;

typedef struct {
  int len;
  double *v; //[M];
} Vector;

typedef struct {
  int rows; int cols;
  double *m; //double m[N*N];
} Matrix;

typedef struct {
  int len;
  Matrix ** m; //Matrix m[M];
} Matrix_List;

typedef struct {
  double err;
  Matrix *D;
  Vector *y;
} Eq_Sol;

typedef struct {
  int feasible;
  Matrix *X;
  Vector *y;
} Solution;

Matrix *create_matrix(int rows, int cols, double *a);
void free_matrix(Matrix *mat);
Vector *create_vector(int l, double *a);
void free_vector(Vector *vec);
Matrix_List *create_matrix_list(int m, int n, double *a);
void sdp_solve(int n, int m, double *c, double *x, double *a, double *b, double *sol);
double get(int i, int j, Matrix *mat);
int d_equal(double a, double b);
int d_equal_ep(double a, double b,double ep);
void print_mat(Matrix *X);
void print_vec(Vector *v);
double abs_val(double x);
double pow_2(double b);
double sqrt_val(double n);
double dot(Matrix *A, Matrix *B);
double vec_comb(Vector *a, Vector *b);
Matrix *scal_div(Matrix *A, double s);
Matrix *mat_mul(Matrix *A, Matrix *B);
Vector *vec_mul(Matrix *A, Vector *b);
double vec_vec_mul(Vector *a, Vector *b);
Matrix *scal_mul(Matrix *A, double s);
Matrix *mat_add(Matrix *A, Matrix *B);
Matrix *mat_sub(Matrix *A, Matrix *B);
Matrix *mat_comb(Vector *y, Matrix_List *A);
Matrix *transpose(Matrix *A);
Vector *vec_sub(Vector *a, Vector *b);
Vector *vectorize(Matrix *A);
void swap_rows(Matrix *X, int a, int b);
void div_row(Matrix *X, int a, double s);
Matrix *flatten(Matrix *X);
void sub_multd_row(Matrix *X, int a, int b, double s);
double norm_vec_big(Vector *v);
double norm_circ(Vector *e, Matrix *LL, Vector *z);
double norm_mat_circ(Matrix *L, Matrix *D);
double norm_mat(Matrix *X);
Matrix *cholesky(Matrix *X);
int psd(Matrix *X);
Solution *sdp(int N, int M, Matrix *C, Matrix *Xp, Matrix_List *A, Vector *b, double theta, double beta);
