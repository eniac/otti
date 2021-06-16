#include <stdio.h>

typedef float fixed_point_precision_16_16;

static const fixed_point_precision_16_16 epsilon   = (fixed_point_precision_16_16)1.0e-5;
static int valid = 1;

	#define N 2
	#define M 2


typedef struct {
  fixed_point_precision_16_16 m0; fixed_point_precision_16_16 m1; 

} Vector;

typedef struct {
  int len;
  fixed_point_precision_16_16 m0; fixed_point_precision_16_16 m1; fixed_point_precision_16_16 m2; fixed_point_precision_16_16 m3; fixed_point_precision_16_16 m4; fixed_point_precision_16_16 m5; fixed_point_precision_16_16 m6; fixed_point_precision_16_16 m7; fixed_point_precision_16_16 m8; fixed_point_precision_16_16 m9; fixed_point_precision_16_16 m10; fixed_point_precision_16_16 m11; fixed_point_precision_16_16 m12; fixed_point_precision_16_16 m13; fixed_point_precision_16_16 m14; fixed_point_precision_16_16 m15; fixed_point_precision_16_16 m16; fixed_point_precision_16_16 m17; fixed_point_precision_16_16 m18; fixed_point_precision_16_16 m19; fixed_point_precision_16_16 m20; fixed_point_precision_16_16 m21; fixed_point_precision_16_16 m22; fixed_point_precision_16_16 m23; fixed_point_precision_16_16 m24; fixed_point_precision_16_16 m25; fixed_point_precision_16_16 m26; fixed_point_precision_16_16 m27; fixed_point_precision_16_16 m28; fixed_point_precision_16_16 m29; fixed_point_precision_16_16 m30; fixed_point_precision_16_16 m31; fixed_point_precision_16_16 m32; fixed_point_precision_16_16 m33; fixed_point_precision_16_16 m34; fixed_point_precision_16_16 m35; 

} Big_Vector;

typedef struct {
  fixed_point_precision_16_16 m0; fixed_point_precision_16_16 m1; fixed_point_precision_16_16 m2; fixed_point_precision_16_16 m3; 

} Matrix;

typedef struct {
  int rows;
  int cols;
  fixed_point_precision_16_16 m0; fixed_point_precision_16_16 m1; fixed_point_precision_16_16 m2; fixed_point_precision_16_16 m3; fixed_point_precision_16_16 m4; fixed_point_precision_16_16 m5; fixed_point_precision_16_16 m6; fixed_point_precision_16_16 m7; fixed_point_precision_16_16 m8; fixed_point_precision_16_16 m9; fixed_point_precision_16_16 m10; fixed_point_precision_16_16 m11; fixed_point_precision_16_16 m12; fixed_point_precision_16_16 m13; fixed_point_precision_16_16 m14; fixed_point_precision_16_16 m15; fixed_point_precision_16_16 m16; fixed_point_precision_16_16 m17; fixed_point_precision_16_16 m18; fixed_point_precision_16_16 m19; fixed_point_precision_16_16 m20; fixed_point_precision_16_16 m21; fixed_point_precision_16_16 m22; fixed_point_precision_16_16 m23; fixed_point_precision_16_16 m24; fixed_point_precision_16_16 m25; fixed_point_precision_16_16 m26; fixed_point_precision_16_16 m27; fixed_point_precision_16_16 m28; fixed_point_precision_16_16 m29; fixed_point_precision_16_16 m30; fixed_point_precision_16_16 m31; fixed_point_precision_16_16 m32; fixed_point_precision_16_16 m33; fixed_point_precision_16_16 m34; fixed_point_precision_16_16 m35; 

} Big_Matrix;

typedef struct {
  	Matrix m0; 	Matrix m1; 

} Matrix_List;

typedef struct {
  	Big_Matrix m0; 	Big_Matrix m1; 

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


Solution sdp(Matrix C, Matrix Xp, Matrix_List A, Vector b, fixed_point_precision_16_16 theta, fixed_point_precision_16_16 beta);
fixed_point_precision_16_16 norm_vec_big(Big_Vector v);
fixed_point_precision_16_16 norm_mat(Matrix X);
fixed_point_precision_16_16 sqrt_val(fixed_point_precision_16_16 n);


void print_mat(Matrix X) {

  printf("\n-----------------\n");

  	printf(" %6.3f", X.m0);
	printf(" %6.3f", X.m1);
printf("\n");	printf(" %6.3f", X.m2);
	printf(" %6.3f", X.m3);
printf("\n");

  printf("\n----------------\n");

}


void print_mat_b(Big_Matrix X) {

  printf("\n-----------------\n");

  	printf(" %6.3f", X.m0);
	printf(" %6.3f", X.m1);
	printf(" %6.3f", X.m2);
	printf(" %6.3f", X.m3);
	printf(" %6.3f", X.m4);
	printf(" %6.3f", X.m5);
printf("\n");	printf(" %6.3f", X.m6);
	printf(" %6.3f", X.m7);
	printf(" %6.3f", X.m8);
	printf(" %6.3f", X.m9);
	printf(" %6.3f", X.m10);
	printf(" %6.3f", X.m11);
printf("\n");	printf(" %6.3f", X.m12);
	printf(" %6.3f", X.m13);
	printf(" %6.3f", X.m14);
	printf(" %6.3f", X.m15);
	printf(" %6.3f", X.m16);
	printf(" %6.3f", X.m17);
printf("\n");	printf(" %6.3f", X.m18);
	printf(" %6.3f", X.m19);
	printf(" %6.3f", X.m20);
	printf(" %6.3f", X.m21);
	printf(" %6.3f", X.m22);
	printf(" %6.3f", X.m23);
printf("\n");	printf(" %6.3f", X.m24);
	printf(" %6.3f", X.m25);
	printf(" %6.3f", X.m26);
	printf(" %6.3f", X.m27);
	printf(" %6.3f", X.m28);
	printf(" %6.3f", X.m29);
printf("\n");	printf(" %6.3f", X.m30);
	printf(" %6.3f", X.m31);
	printf(" %6.3f", X.m32);
	printf(" %6.3f", X.m33);
	printf(" %6.3f", X.m34);
	printf(" %6.3f", X.m35);
printf("\n");

  printf("\n----------------\n");
}

void print_vec(Vector v) {

  printf("\n-----------------\n");


  	printf(" %6.3f", v.m0);
printf("\n");	printf(" %6.3f", v.m1);
printf("\n");

  printf("\n----------------\n");
}

void print_vec_b(Big_Vector v) {

  printf("\n-----------------\n");

  	printf(" %6.3f", v.m0);
printf("\n");	printf(" %6.3f", v.m1);
printf("\n");	printf(" %6.3f", v.m2);
printf("\n");	printf(" %6.3f", v.m3);
printf("\n");	printf(" %6.3f", v.m4);
printf("\n");	printf(" %6.3f", v.m5);
printf("\n");	printf(" %6.3f", v.m6);
printf("\n");	printf(" %6.3f", v.m7);
printf("\n");	printf(" %6.3f", v.m8);
printf("\n");	printf(" %6.3f", v.m9);
printf("\n");	printf(" %6.3f", v.m10);
printf("\n");	printf(" %6.3f", v.m11);
printf("\n");	printf(" %6.3f", v.m12);
printf("\n");	printf(" %6.3f", v.m13);
printf("\n");	printf(" %6.3f", v.m14);
printf("\n");	printf(" %6.3f", v.m15);
printf("\n");	printf(" %6.3f", v.m16);
printf("\n");	printf(" %6.3f", v.m17);
printf("\n");	printf(" %6.3f", v.m18);
printf("\n");	printf(" %6.3f", v.m19);
printf("\n");	printf(" %6.3f", v.m20);
printf("\n");	printf(" %6.3f", v.m21);
printf("\n");	printf(" %6.3f", v.m22);
printf("\n");	printf(" %6.3f", v.m23);
printf("\n");	printf(" %6.3f", v.m24);
printf("\n");	printf(" %6.3f", v.m25);
printf("\n");	printf(" %6.3f", v.m26);
printf("\n");	printf(" %6.3f", v.m27);
printf("\n");	printf(" %6.3f", v.m28);
printf("\n");	printf(" %6.3f", v.m29);
printf("\n");	printf(" %6.3f", v.m30);
printf("\n");	printf(" %6.3f", v.m31);
printf("\n");	printf(" %6.3f", v.m32);
printf("\n");	printf(" %6.3f", v.m33);
printf("\n");	printf(" %6.3f", v.m34);
printf("\n");	printf(" %6.3f", v.m35);
printf("\n");

  printf("\n----------------\n");
}


int main(void) {


  // problem

 Matrix C = {-0.1983367,  0.54620727, 0.54620727,  0.29183634};
  Vector b = {-2.3830572764539832, 0.8521208961278653};
  Matrix X = {1.3713053, 0.16070848,0.16070848, 1.43693619};

  Matrix A0 = {-0.99890972, 0.14410886,0.14410886, -0.73737868};
  Matrix A1 = {0.14047667, -0.17714865,-0.17714865,  0.49857682};
  Matrix_List A = {A0,A1};

  //printf("%6f\n",sqrt_val(17));


  Solution Q = sdp(C,X,A,b,1.0,0.25);
  print_mat(Q.X);


 //sdp_check(C, Q.X, A, b, Q.y, Q.feasible));

}

/*
fixed_point_precision_16_16 get(int i, int j, Matrix mat){
  return mat.m[i*(mat.cols)+j];
}

fixed_point_precision_16_16 get_big(int i, int j, Big_Matrix mat){
  return mat.m[i*(mat.cols)+j];
}
*/

int d_equal(fixed_point_precision_16_16 a, fixed_point_precision_16_16 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b) < epsilon;
  }
}

int d_equal_ep(fixed_point_precision_16_16 a, fixed_point_precision_16_16 b,fixed_point_precision_16_16 ep){
  if ((a-b) > 0) {
    return (a-b) < ep;
  } else {
    return -1*(a-b) < ep;
  }
}







fixed_point_precision_16_16 abs_val(fixed_point_precision_16_16 x){
  //printf("ABS_VAL %6f", x);

  if (x < (fixed_point_precision_16_16)0.0){
    return x*-1;
  } else {
    return x;
  }
}
fixed_point_precision_16_16 sqr(fixed_point_precision_16_16 x) {
  return x * x;
}

fixed_point_precision_16_16 pow_2(fixed_point_precision_16_16 b)
{
  return (b*b);
}


fixed_point_precision_16_16 sqrt_val(fixed_point_precision_16_16 n)
  {
      printf("SQRT of %6f\n",n);

      fixed_point_precision_16_16 x = n;

      fixed_point_precision_16_16 root;

      int count = 0;

      while (1)
      {
          count++;

          root = 0.5 * (x + (n / x));

          // Check for closeness
          if (abs_val(root - x) < epsilon)
              break;

          // Update root
          x = root;
      }

      return root;
  }


fixed_point_precision_16_16 dot(Matrix A, Matrix B){
  fixed_point_precision_16_16 s = 0;
	s = s + (A.m0 * B.m0);
	s = s + (A.m1 * B.m1);
	s = s + (A.m2 * B.m2);
	s = s + (A.m3 * B.m3);

  return s;
}

fixed_point_precision_16_16 vec_comb(Vector a, Vector b){
  fixed_point_precision_16_16 s = 0;
	s = s + (a.m0 * b.m0);
	s = s + (a.m1 * b.m1);


  return s;
}

Matrix scal_div(Matrix A, fixed_point_precision_16_16 s){
	A.m0 = A.m0 / s;
	A.m1 = A.m1 / s;

  return A;
}

Matrix mat_mul(Matrix A, Matrix B){
Matrix C = {0,0,0,0};

	C.m0 += A.m0 * B.m0;
	C.m0 += A.m0 * B.m2;
	C.m1 += A.m1 * B.m1;
	C.m1 += A.m1 * B.m3;
	C.m2 += A.m2 * B.m0;
	C.m2 += A.m2 * B.m2;
	C.m3 += A.m3 * B.m1;
	C.m3 += A.m3 * B.m3;

  return C;
}

Big_Matrix mat_mul_big(Big_Matrix A, Big_Matrix B){
Big_Matrix C = {A.rows,A.cols, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

	C.m0 += A.m0 * B.m0;
	C.m0 += A.m0 * B.m2;
	C.m0 += A.m0 * B.m4;
	C.m0 += A.m0 * B.m6;
	C.m0 += A.m0 * B.m8;
	C.m0 += A.m0 * B.m10;
	C.m1 += A.m1 * B.m1;
	C.m1 += A.m1 * B.m3;
	C.m1 += A.m1 * B.m5;
	C.m1 += A.m1 * B.m7;
	C.m1 += A.m1 * B.m9;
	C.m1 += A.m1 * B.m11;
	C.m2 += A.m2 * B.m2;
	C.m2 += A.m2 * B.m4;
	C.m2 += A.m2 * B.m6;
	C.m2 += A.m2 * B.m8;
	C.m2 += A.m2 * B.m10;
	C.m2 += A.m2 * B.m12;
	C.m3 += A.m3 * B.m3;
	C.m3 += A.m3 * B.m5;
	C.m3 += A.m3 * B.m7;
	C.m3 += A.m3 * B.m9;
	C.m3 += A.m3 * B.m11;
	C.m3 += A.m3 * B.m13;
	C.m4 += A.m4 * B.m4;
	C.m4 += A.m4 * B.m6;
	C.m4 += A.m4 * B.m8;
	C.m4 += A.m4 * B.m10;
	C.m4 += A.m4 * B.m12;
	C.m4 += A.m4 * B.m14;
	C.m5 += A.m5 * B.m5;
	C.m5 += A.m5 * B.m7;
	C.m5 += A.m5 * B.m9;
	C.m5 += A.m5 * B.m11;
	C.m5 += A.m5 * B.m13;
	C.m5 += A.m5 * B.m15;
	C.m2 += A.m2 * B.m0;
	C.m2 += A.m2 * B.m2;
	C.m2 += A.m2 * B.m4;
	C.m2 += A.m2 * B.m6;
	C.m2 += A.m2 * B.m8;
	C.m2 += A.m2 * B.m10;
	C.m3 += A.m3 * B.m1;
	C.m3 += A.m3 * B.m3;
	C.m3 += A.m3 * B.m5;
	C.m3 += A.m3 * B.m7;
	C.m3 += A.m3 * B.m9;
	C.m3 += A.m3 * B.m11;
	C.m4 += A.m4 * B.m2;
	C.m4 += A.m4 * B.m4;
	C.m4 += A.m4 * B.m6;
	C.m4 += A.m4 * B.m8;
	C.m4 += A.m4 * B.m10;
	C.m4 += A.m4 * B.m12;
	C.m5 += A.m5 * B.m3;
	C.m5 += A.m5 * B.m5;
	C.m5 += A.m5 * B.m7;
	C.m5 += A.m5 * B.m9;
	C.m5 += A.m5 * B.m11;
	C.m5 += A.m5 * B.m13;
	C.m6 += A.m6 * B.m4;
	C.m6 += A.m6 * B.m6;
	C.m6 += A.m6 * B.m8;
	C.m6 += A.m6 * B.m10;
	C.m6 += A.m6 * B.m12;
	C.m6 += A.m6 * B.m14;
	C.m7 += A.m7 * B.m5;
	C.m7 += A.m7 * B.m7;
	C.m7 += A.m7 * B.m9;
	C.m7 += A.m7 * B.m11;
	C.m7 += A.m7 * B.m13;
	C.m7 += A.m7 * B.m15;
	C.m4 += A.m4 * B.m0;
	C.m4 += A.m4 * B.m2;
	C.m4 += A.m4 * B.m4;
	C.m4 += A.m4 * B.m6;
	C.m4 += A.m4 * B.m8;
	C.m4 += A.m4 * B.m10;
	C.m5 += A.m5 * B.m1;
	C.m5 += A.m5 * B.m3;
	C.m5 += A.m5 * B.m5;
	C.m5 += A.m5 * B.m7;
	C.m5 += A.m5 * B.m9;
	C.m5 += A.m5 * B.m11;
	C.m6 += A.m6 * B.m2;
	C.m6 += A.m6 * B.m4;
	C.m6 += A.m6 * B.m6;
	C.m6 += A.m6 * B.m8;
	C.m6 += A.m6 * B.m10;
	C.m6 += A.m6 * B.m12;
	C.m7 += A.m7 * B.m3;
	C.m7 += A.m7 * B.m5;
	C.m7 += A.m7 * B.m7;
	C.m7 += A.m7 * B.m9;
	C.m7 += A.m7 * B.m11;
	C.m7 += A.m7 * B.m13;
	C.m8 += A.m8 * B.m4;
	C.m8 += A.m8 * B.m6;
	C.m8 += A.m8 * B.m8;
	C.m8 += A.m8 * B.m10;
	C.m8 += A.m8 * B.m12;
	C.m8 += A.m8 * B.m14;
	C.m9 += A.m9 * B.m5;
	C.m9 += A.m9 * B.m7;
	C.m9 += A.m9 * B.m9;
	C.m9 += A.m9 * B.m11;
	C.m9 += A.m9 * B.m13;
	C.m9 += A.m9 * B.m15;
	C.m6 += A.m6 * B.m0;
	C.m6 += A.m6 * B.m2;
	C.m6 += A.m6 * B.m4;
	C.m6 += A.m6 * B.m6;
	C.m6 += A.m6 * B.m8;
	C.m6 += A.m6 * B.m10;
	C.m7 += A.m7 * B.m1;
	C.m7 += A.m7 * B.m3;
	C.m7 += A.m7 * B.m5;
	C.m7 += A.m7 * B.m7;
	C.m7 += A.m7 * B.m9;
	C.m7 += A.m7 * B.m11;
	C.m8 += A.m8 * B.m2;
	C.m8 += A.m8 * B.m4;
	C.m8 += A.m8 * B.m6;
	C.m8 += A.m8 * B.m8;
	C.m8 += A.m8 * B.m10;
	C.m8 += A.m8 * B.m12;
	C.m9 += A.m9 * B.m3;
	C.m9 += A.m9 * B.m5;
	C.m9 += A.m9 * B.m7;
	C.m9 += A.m9 * B.m9;
	C.m9 += A.m9 * B.m11;
	C.m9 += A.m9 * B.m13;
	C.m10 += A.m10 * B.m4;
	C.m10 += A.m10 * B.m6;
	C.m10 += A.m10 * B.m8;
	C.m10 += A.m10 * B.m10;
	C.m10 += A.m10 * B.m12;
	C.m10 += A.m10 * B.m14;
	C.m11 += A.m11 * B.m5;
	C.m11 += A.m11 * B.m7;
	C.m11 += A.m11 * B.m9;
	C.m11 += A.m11 * B.m11;
	C.m11 += A.m11 * B.m13;
	C.m11 += A.m11 * B.m15;
	C.m8 += A.m8 * B.m0;
	C.m8 += A.m8 * B.m2;
	C.m8 += A.m8 * B.m4;
	C.m8 += A.m8 * B.m6;
	C.m8 += A.m8 * B.m8;
	C.m8 += A.m8 * B.m10;
	C.m9 += A.m9 * B.m1;
	C.m9 += A.m9 * B.m3;
	C.m9 += A.m9 * B.m5;
	C.m9 += A.m9 * B.m7;
	C.m9 += A.m9 * B.m9;
	C.m9 += A.m9 * B.m11;
	C.m10 += A.m10 * B.m2;
	C.m10 += A.m10 * B.m4;
	C.m10 += A.m10 * B.m6;
	C.m10 += A.m10 * B.m8;
	C.m10 += A.m10 * B.m10;
	C.m10 += A.m10 * B.m12;
	C.m11 += A.m11 * B.m3;
	C.m11 += A.m11 * B.m5;
	C.m11 += A.m11 * B.m7;
	C.m11 += A.m11 * B.m9;
	C.m11 += A.m11 * B.m11;
	C.m11 += A.m11 * B.m13;
	C.m12 += A.m12 * B.m4;
	C.m12 += A.m12 * B.m6;
	C.m12 += A.m12 * B.m8;
	C.m12 += A.m12 * B.m10;
	C.m12 += A.m12 * B.m12;
	C.m12 += A.m12 * B.m14;
	C.m13 += A.m13 * B.m5;
	C.m13 += A.m13 * B.m7;
	C.m13 += A.m13 * B.m9;
	C.m13 += A.m13 * B.m11;
	C.m13 += A.m13 * B.m13;
	C.m13 += A.m13 * B.m15;
	C.m10 += A.m10 * B.m0;
	C.m10 += A.m10 * B.m2;
	C.m10 += A.m10 * B.m4;
	C.m10 += A.m10 * B.m6;
	C.m10 += A.m10 * B.m8;
	C.m10 += A.m10 * B.m10;
	C.m11 += A.m11 * B.m1;
	C.m11 += A.m11 * B.m3;
	C.m11 += A.m11 * B.m5;
	C.m11 += A.m11 * B.m7;
	C.m11 += A.m11 * B.m9;
	C.m11 += A.m11 * B.m11;
	C.m12 += A.m12 * B.m2;
	C.m12 += A.m12 * B.m4;
	C.m12 += A.m12 * B.m6;
	C.m12 += A.m12 * B.m8;
	C.m12 += A.m12 * B.m10;
	C.m12 += A.m12 * B.m12;
	C.m13 += A.m13 * B.m3;
	C.m13 += A.m13 * B.m5;
	C.m13 += A.m13 * B.m7;
	C.m13 += A.m13 * B.m9;
	C.m13 += A.m13 * B.m11;
	C.m13 += A.m13 * B.m13;
	C.m14 += A.m14 * B.m4;
	C.m14 += A.m14 * B.m6;
	C.m14 += A.m14 * B.m8;
	C.m14 += A.m14 * B.m10;
	C.m14 += A.m14 * B.m12;
	C.m14 += A.m14 * B.m14;
	C.m15 += A.m15 * B.m5;
	C.m15 += A.m15 * B.m7;
	C.m15 += A.m15 * B.m9;
	C.m15 += A.m15 * B.m11;
	C.m15 += A.m15 * B.m13;
	C.m15 += A.m15 * B.m15;

  return C;
}

//TODO CHECK
Vector vec_mul(Matrix A, Vector b){
  Vector c = {0,0};

	c.m0 += A.m0 * b.m0;
	c.m0 += A.m1 * b.m1;
	c.m1 += A.m2 * b.m0;
	c.m1 += A.m3 * b.m1;

  return c;
}

//TODO CHECK
Big_Vector vec_mul_big(Big_Matrix A, Big_Vector b){
Big_Vector c = {b.len, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

	c.m0 += A.m0 * b.m0;
	c.m0 += A.m1 * b.m1;
	c.m0 += A.m2 * b.m2;
	c.m0 += A.m3 * b.m3;
	c.m0 += A.m4 * b.m4;
	c.m0 += A.m5 * b.m5;
	c.m1 += A.m2 * b.m0;
	c.m1 += A.m3 * b.m1;
	c.m1 += A.m4 * b.m2;
	c.m1 += A.m5 * b.m3;
	c.m1 += A.m6 * b.m4;
	c.m1 += A.m7 * b.m5;
	c.m2 += A.m4 * b.m0;
	c.m2 += A.m5 * b.m1;
	c.m2 += A.m6 * b.m2;
	c.m2 += A.m7 * b.m3;
	c.m2 += A.m8 * b.m4;
	c.m2 += A.m9 * b.m5;
	c.m3 += A.m6 * b.m0;
	c.m3 += A.m7 * b.m1;
	c.m3 += A.m8 * b.m2;
	c.m3 += A.m9 * b.m3;
	c.m3 += A.m10 * b.m4;
	c.m3 += A.m11 * b.m5;
	c.m4 += A.m8 * b.m0;
	c.m4 += A.m9 * b.m1;
	c.m4 += A.m10 * b.m2;
	c.m4 += A.m11 * b.m3;
	c.m4 += A.m12 * b.m4;
	c.m4 += A.m13 * b.m5;
	c.m5 += A.m10 * b.m0;
	c.m5 += A.m11 * b.m1;
	c.m5 += A.m12 * b.m2;
	c.m5 += A.m13 * b.m3;
	c.m5 += A.m14 * b.m4;
	c.m5 += A.m15 * b.m5;

  return c;
}

fixed_point_precision_16_16 vec_vec_mul(Vector a, Vector b){
  fixed_point_precision_16_16 c = 0.0;
	c += a.m0 * b.m0;
	c += a.m1 * b.m1;

  return c;

}

Matrix scal_mul(Matrix A, fixed_point_precision_16_16 s){
	A.m0 = A.m0 * s;
	A.m1 = A.m1 * s;
	A.m2 = A.m2 * s;
	A.m3 = A.m3 * s;

  return A;
}

Matrix mat_add(Matrix A, Matrix B){
  Matrix C = A;
	C.m0 = A.m0 + B.m0;
	C.m1 = A.m1 + B.m1;
	C.m2 = A.m2 + B.m2;
	C.m3 = A.m3 + B.m3;

  return C;
}


Big_Matrix mat_add_big(Big_Matrix A, Big_Matrix B){
  Big_Matrix C = A;
	C.m0 = A.m0 + B.m0;
	C.m1 = A.m1 + B.m1;
	C.m2 = A.m2 + B.m2;
	C.m3 = A.m3 + B.m3;
	C.m4 = A.m4 + B.m4;
	C.m5 = A.m5 + B.m5;
	C.m6 = A.m6 + B.m6;
	C.m7 = A.m7 + B.m7;
	C.m8 = A.m8 + B.m8;
	C.m9 = A.m9 + B.m9;
	C.m10 = A.m10 + B.m10;
	C.m11 = A.m11 + B.m11;
	C.m12 = A.m12 + B.m12;
	C.m13 = A.m13 + B.m13;
	C.m14 = A.m14 + B.m14;
	C.m15 = A.m15 + B.m15;
	C.m16 = A.m16 + B.m16;
	C.m17 = A.m17 + B.m17;
	C.m18 = A.m18 + B.m18;
	C.m19 = A.m19 + B.m19;
	C.m20 = A.m20 + B.m20;
	C.m21 = A.m21 + B.m21;
	C.m22 = A.m22 + B.m22;
	C.m23 = A.m23 + B.m23;
	C.m24 = A.m24 + B.m24;
	C.m25 = A.m25 + B.m25;
	C.m26 = A.m26 + B.m26;
	C.m27 = A.m27 + B.m27;
	C.m28 = A.m28 + B.m28;
	C.m29 = A.m29 + B.m29;
	C.m30 = A.m30 + B.m30;
	C.m31 = A.m31 + B.m31;
	C.m32 = A.m32 + B.m32;
	C.m33 = A.m33 + B.m33;
	C.m34 = A.m34 + B.m34;
	C.m35 = A.m35 + B.m35;

  return C;
}

Matrix mat_sub(Matrix A, Matrix B){
  Matrix C = A;
	C.m0 = A.m0 - B.m0;
	C.m1 = A.m1 - B.m1;
	C.m2 = A.m2 - B.m2;
	C.m3 = A.m3 - B.m3;

  return C;
}

Big_Matrix mat_sub_big(Big_Matrix A, Big_Matrix B){
  Big_Matrix C = A;
	C.m0 = A.m0 - B.m0;
	C.m1 = A.m1 - B.m1;
	C.m2 = A.m2 - B.m2;
	C.m3 = A.m3 - B.m3;
	C.m4 = A.m4 - B.m4;
	C.m5 = A.m5 - B.m5;
	C.m6 = A.m6 - B.m6;
	C.m7 = A.m7 - B.m7;
	C.m8 = A.m8 - B.m8;
	C.m9 = A.m9 - B.m9;
	C.m10 = A.m10 - B.m10;
	C.m11 = A.m11 - B.m11;
	C.m12 = A.m12 - B.m12;
	C.m13 = A.m13 - B.m13;
	C.m14 = A.m14 - B.m14;
	C.m15 = A.m15 - B.m15;
	C.m16 = A.m16 - B.m16;
	C.m17 = A.m17 - B.m17;
	C.m18 = A.m18 - B.m18;
	C.m19 = A.m19 - B.m19;
	C.m20 = A.m20 - B.m20;
	C.m21 = A.m21 - B.m21;
	C.m22 = A.m22 - B.m22;
	C.m23 = A.m23 - B.m23;
	C.m24 = A.m24 - B.m24;
	C.m25 = A.m25 - B.m25;
	C.m26 = A.m26 - B.m26;
	C.m27 = A.m27 - B.m27;
	C.m28 = A.m28 - B.m28;
	C.m29 = A.m29 - B.m29;
	C.m30 = A.m30 - B.m30;
	C.m31 = A.m31 - B.m31;
	C.m32 = A.m32 - B.m32;
	C.m33 = A.m33 - B.m33;
	C.m34 = A.m34 - B.m34;
	C.m35 = A.m35 - B.m35;

  return C;
}

Matrix mat_comb(Vector y, Matrix_List A){
  Matrix sum = scal_mul(A.m0, y.m0);
	sum = mat_add(sum,scal_mul(A.m1, y.m1 ));

  return sum;

}

Matrix transpose(Matrix A){
  Matrix T = A;
	T.m0 = A.m0;
	T.m1 = A.m2;
	T.m2 = A.m1;
	T.m3 = A.m3;

  return T;
}

Vector vec_sub(Vector a, Vector b){
  Vector x = a;
	x.m0 = a.m0 - b.m0;
	x.m1 = a.m1 - b.m1;

  return x;
}

Big_Vector vec_sub_big(Big_Vector a, Big_Vector b){
  Big_Vector x = a;
	x.m0 = a.m0 - b.m0;
	x.m1 = a.m1 - b.m1;
	x.m2 = a.m2 - b.m2;
	x.m3 = a.m3 - b.m3;
	x.m4 = a.m4 - b.m4;
	x.m5 = a.m5 - b.m5;
	x.m6 = a.m6 - b.m6;
	x.m7 = a.m7 - b.m7;
	x.m8 = a.m8 - b.m8;
	x.m9 = a.m9 - b.m9;
	x.m10 = a.m10 - b.m10;
	x.m11 = a.m11 - b.m11;
	x.m12 = a.m12 - b.m12;
	x.m13 = a.m13 - b.m13;
	x.m14 = a.m14 - b.m14;
	x.m15 = a.m15 - b.m15;
	x.m16 = a.m16 - b.m16;
	x.m17 = a.m17 - b.m17;
	x.m18 = a.m18 - b.m18;
	x.m19 = a.m19 - b.m19;
	x.m20 = a.m20 - b.m20;
	x.m21 = a.m21 - b.m21;
	x.m22 = a.m22 - b.m22;
	x.m23 = a.m23 - b.m23;
	x.m24 = a.m24 - b.m24;
	x.m25 = a.m25 - b.m25;
	x.m26 = a.m26 - b.m26;
	x.m27 = a.m27 - b.m27;
	x.m28 = a.m28 - b.m28;
	x.m29 = a.m29 - b.m29;
	x.m30 = a.m30 - b.m30;
	x.m31 = a.m31 - b.m31;
	x.m32 = a.m32 - b.m32;
	x.m33 = a.m33 - b.m33;
	x.m34 = a.m34 - b.m34;
	x.m35 = a.m35 - b.m35;

  return x;
}

Big_Vector vectorize(Big_Matrix A){
Big_Vector v = {A.rows*A.cols, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

	v.m0 = A.m0;
	v.m1 = A.m1;
	v.m2 = A.m2;
	v.m3 = A.m3;
	v.m4 = A.m4;
	v.m5 = A.m5;
	v.m6 = A.m6;
	v.m7 = A.m7;
	v.m8 = A.m8;
	v.m9 = A.m9;
	v.m10 = A.m10;
	v.m11 = A.m11;
	v.m12 = A.m12;
	v.m13 = A.m13;
	v.m14 = A.m14;
	v.m15 = A.m15;
	v.m16 = A.m16;
	v.m17 = A.m17;
	v.m18 = A.m18;
	v.m19 = A.m19;
	v.m20 = A.m20;
	v.m21 = A.m21;
	v.m22 = A.m22;
	v.m23 = A.m23;
	v.m24 = A.m24;
	v.m25 = A.m25;
	v.m26 = A.m26;
	v.m27 = A.m27;
	v.m28 = A.m28;
	v.m29 = A.m29;
	v.m30 = A.m30;
	v.m31 = A.m31;
	v.m32 = A.m32;
	v.m33 = A.m33;
	v.m34 = A.m34;
	v.m35 = A.m35;

  return v;
}

Big_Matrix biggify_mat(Matrix P, int rows, int cols){
Big_Matrix A = {(N*N+M),(N*N+M), 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

	A.m0 = P.m0;
	A.m1 = P.m1;
	A.m2 = P.m2;
	A.m3 = P.m3;

  return A;

}


fixed_point_precision_16_16 norm_circ(Big_Vector e, Big_Matrix LL, Big_Vector z){
  //Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
  Big_Vector res = {e.len, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


  	res.m0 += LL.m0 * z.m0;
	res.m0 += LL.m1 * z.m1;
	res.m0 += LL.m2 * z.m2;
	res.m0 += LL.m3 * z.m3;
	res.m0 += LL.m4 * z.m4;
	res.m0 += LL.m5 * z.m5;
	res.m1 += LL.m6 * z.m0;
	res.m1 += LL.m7 * z.m1;
	res.m1 += LL.m8 * z.m2;
	res.m1 += LL.m9 * z.m3;
	res.m1 += LL.m10 * z.m4;
	res.m1 += LL.m11 * z.m5;
	res.m2 += LL.m12 * z.m0;
	res.m2 += LL.m13 * z.m1;
	res.m2 += LL.m14 * z.m2;
	res.m2 += LL.m15 * z.m3;
	res.m2 += LL.m16 * z.m4;
	res.m2 += LL.m17 * z.m5;
	res.m3 += LL.m18 * z.m0;
	res.m3 += LL.m19 * z.m1;
	res.m3 += LL.m20 * z.m2;
	res.m3 += LL.m21 * z.m3;
	res.m3 += LL.m22 * z.m4;
	res.m3 += LL.m23 * z.m5;
	res.m4 += LL.m24 * z.m0;
	res.m4 += LL.m25 * z.m1;
	res.m4 += LL.m26 * z.m2;
	res.m4 += LL.m27 * z.m3;
	res.m4 += LL.m28 * z.m4;
	res.m4 += LL.m29 * z.m5;
	res.m5 += LL.m30 * z.m0;
	res.m5 += LL.m31 * z.m1;
	res.m5 += LL.m32 * z.m2;
	res.m5 += LL.m33 * z.m3;
	res.m5 += LL.m34 * z.m4;
	res.m5 += LL.m35 * z.m5;
	res.m0 = e.m0 - res.m0;
	res.m1 = e.m1 - res.m1;
	res.m2 = e.m2 - res.m2;
	res.m3 = e.m3 - res.m3;
	res.m4 = e.m4 - res.m4;
	res.m5 = e.m5 - res.m5;


  printf("pre norm vec big\n");
  fixed_point_precision_16_16 err = norm_vec_big(res);
  return err;

}

fixed_point_precision_16_16 norm_vec_big(Big_Vector v){

  fixed_point_precision_16_16 sum = 0.0;
	sum = sum + pow_2(v.m0);
	sum = sum + pow_2(v.m1);
	sum = sum + pow_2(v.m2);
	sum = sum + pow_2(v.m3);
	sum = sum + pow_2(v.m4);
	sum = sum + pow_2(v.m5);
	sum = sum + pow_2(v.m6);
	sum = sum + pow_2(v.m7);
	sum = sum + pow_2(v.m8);
	sum = sum + pow_2(v.m9);
	sum = sum + pow_2(v.m10);
	sum = sum + pow_2(v.m11);
	sum = sum + pow_2(v.m12);
	sum = sum + pow_2(v.m13);
	sum = sum + pow_2(v.m14);
	sum = sum + pow_2(v.m15);
	sum = sum + pow_2(v.m16);
	sum = sum + pow_2(v.m17);
	sum = sum + pow_2(v.m18);
	sum = sum + pow_2(v.m19);
	sum = sum + pow_2(v.m20);
	sum = sum + pow_2(v.m21);
	sum = sum + pow_2(v.m22);
	sum = sum + pow_2(v.m23);
	sum = sum + pow_2(v.m24);
	sum = sum + pow_2(v.m25);
	sum = sum + pow_2(v.m26);
	sum = sum + pow_2(v.m27);
	sum = sum + pow_2(v.m28);
	sum = sum + pow_2(v.m29);
	sum = sum + pow_2(v.m30);
	sum = sum + pow_2(v.m31);
	sum = sum + pow_2(v.m32);
	sum = sum + pow_2(v.m33);
	sum = sum + pow_2(v.m34);
	sum = sum + pow_2(v.m35);


  printf("post pows\n");

  fixed_point_precision_16_16 r = sqrt_val(sum);

  printf("post sq\n");
  return r;
}

fixed_point_precision_16_16 norm_mat_circ(Matrix L, Matrix D){ //L,D regular sized
//Matrix I = mat_mul(mat_mul(inverse(L),D),inverse(transpose(L)));

//LI = inverse(L)
//Matrix_List LU_List = LUP_decompose(L);
Matrix LA = {0,0,0,0};

Matrix UA = {0,0,0,0};

Matrix PA = {0,0,0,0};



//pivot A P
	PA.m0 = (fixed_point_precision_16_16)1.0;
	PA.m3 = (fixed_point_precision_16_16)1.0;


	fixed_point_precision_16_16 cmp; int max_ja;
	max_ja = 0;
	if (max_ja == 0){	cmp = L.m0;}
	if (max_ja == 1){	cmp = L.m2;}
	if (abs_val(L.m0) > abs_val(cmp)){ max_ja = 0; }
	if (max_ja == 0){	cmp = L.m0;}
	if (max_ja == 1){	cmp = L.m2;}
	if (abs_val(L.m2) > abs_val(cmp)){ max_ja = 1; }
if (max_ja == 0){}
if (max_ja == 1){	fixed_point_precision_16_16 tempa0;	tempa0 = PA.m0;
	PA.m0 = PA.m2;
	PA.m2 = tempa0;
	tempa0 = PA.m1;
	PA.m1 = PA.m3;
	PA.m3 = tempa0;
}
	max_ja = 1;
	if (max_ja == 0){	cmp = L.m1;}
	if (max_ja == 1){	cmp = L.m3;}
	if (abs_val(L.m1) > abs_val(cmp)){ max_ja = 0; }
	if (max_ja == 0){	cmp = L.m1;}
	if (max_ja == 1){	cmp = L.m3;}
	if (abs_val(L.m3) > abs_val(cmp)){ max_ja = 1; }
if (max_ja == 0){	fixed_point_precision_16_16 tempa1;	tempa1 = PA.m2;
	PA.m2 = PA.m0;
	PA.m0 = tempa1;
	tempa1 = PA.m3;
	PA.m3 = PA.m1;
	PA.m1 = tempa1;
}
if (max_ja == 1){}


//Big_Matrix LLp = mat_mul_big(PD,LL);
Matrix LLp = {0,0,0,0};


	LLp.m0 = PA.m0 * L.m0;
	LLp.m0 = PA.m1 * L.m2;
	LLp.m1 = PA.m0 * L.m1;
	LLp.m1 = PA.m1 * L.m3;
	LLp.m2 = PA.m2 * L.m0;
	LLp.m2 = PA.m3 * L.m2;
	LLp.m3 = PA.m2 * L.m1;
	LLp.m3 = PA.m3 * L.m3;


	LA.m0 = (fixed_point_precision_16_16)1.0;
	LA.m3 = (fixed_point_precision_16_16)1.0;


	fixed_point_precision_16_16 se;
	se = 0;
	UA.m0 = LLp.m0 - se;
	se = 0;
	LA.m0 = (LLp.m0 - se) / UA.m0;
	se = 0;
	se += LA.m2 * UA.m0;
	UA.m2 = LLp.m2 - se;
	se = 0;
	LA.m2 = (LLp.m2 - se) / UA.m0;
	se = 0;
	se += LA.m2 * UA.m1;
	UA.m3 = LLp.m3 - se;
	se = 0;
	se += LA.m2 * UA.m1;
	LA.m3 = (LLp.m3 - se) / UA.m3;


Matrix IDT = {0,0,0,0};

	IDT.m0 = (fixed_point_precision_16_16)1.0;
	IDT.m3 = (fixed_point_precision_16_16)1.0;


//Big_Matrix_List LU_List = {2, , PD};

//Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
Matrix LU = {0,0,0,0};

	LU.m0 = LA.m0 + UA.m0 - IDT.m0;
	LU.m3 = LA.m3 + UA.m3 - IDT.m3;


// end decomp

Matrix P = PA;
Matrix LI = LU;

Big_Vector bb0 = {N, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	bb0.m0 = (fixed_point_precision_16_16)1.0;
Big_Vector b0 = {N, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	b0.m0 += P.m0 * bb0.m0;
	b0.m0 += P.m1 * bb0.m1;
	b0.m1 += P.m2 * bb0.m0;
	b0.m1 += P.m3 * bb0.m1;
	b0.m1 = b0.m1 - LU.m2 * b0.m0;
	b0.m1 = b0.m1 / LU.m3;
	b0.m0 = b0.m0 - LU.m1 * b0.m1;
	LI.m0 = b0.m0;
	LI.m2 = b0.m1;
Big_Vector bb1 = {N, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	bb1.m1 = (fixed_point_precision_16_16)1.0;
Big_Vector b1 = {N, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	b1.m0 += P.m0 * bb1.m0;
	b1.m0 += P.m1 * bb1.m1;
	b1.m1 += P.m2 * bb1.m0;
	b1.m1 += P.m3 * bb1.m1;
	b1.m1 = b1.m1 - LU.m2 * b1.m0;
	b1.m1 = b1.m1 / LU.m3;
	b1.m0 = b1.m0 - LU.m1 * b1.m1;
	LI.m1 = b1.m0;
	LI.m3 = b1.m1;


//LT = transpose(L)
Matrix LT = L;
	LT.m0 = L.m0;
	LT.m1 = L.m2;
	LT.m2 = L.m1;
	LT.m3 = L.m3;


//LTI = inverse(L.T)
//Matrix_List LU_List = LUP_decompose(LT);
Matrix LB = {0,0,0,0};

Matrix UB = {0,0,0,0};

Matrix PB = {0,0,0,0};


//pivot A P
	PB.m0 = (fixed_point_precision_16_16)1.0;
	PB.m3 = (fixed_point_precision_16_16)1.0;


	fixed_point_precision_16_16 cmp2; int max_j2;
	max_j2 = 0;
	if (abs_val(LT.m0) > abs_val(cmp2)){ max_j2 = 0; }
	if (abs_val(LT.m2) > abs_val(cmp2)){ max_j2 = 1; }
if (max_j2 == 0){}
if (max_j2 == 1){	fixed_point_precision_16_16 temp20;
	temp20 = PB.m0;
	PB.m0 = PB.m2;
	PB.m2 = temp20;
	temp20 = PB.m1;
	PB.m1 = PB.m3;
	PB.m3 = temp20;
}
	max_j2 = 1;
	if (abs_val(LT.m1) > abs_val(cmp2)){ max_j2 = 0; }
	if (abs_val(LT.m3) > abs_val(cmp2)){ max_j2 = 1; }
if (max_j2 == 0){	fixed_point_precision_16_16 temp21;
	temp21 = PB.m2;
	PB.m2 = PB.m0;
	PB.m0 = temp21;
	temp21 = PB.m3;
	PB.m3 = PB.m1;
	PB.m1 = temp21;
}
if (max_j2 == 1){}


//Big_Matrix LLp = mat_mul_big(PD,LL);
Matrix LLpp = {0,0,0,0};


	LLpp.m0 = PB.m0 * LT.m0;
	LLpp.m0 = PB.m1 * LT.m2;
	LLpp.m1 = PB.m0 * LT.m1;
	LLpp.m1 = PB.m1 * LT.m3;
	LLpp.m2 = PB.m2 * LT.m0;
	LLpp.m2 = PB.m3 * LT.m2;
	LLpp.m3 = PB.m2 * LT.m1;
	LLpp.m3 = PB.m3 * LT.m3;


	LB.m0 = (fixed_point_precision_16_16)1.0;
	LB.m3 = (fixed_point_precision_16_16)1.0;


	fixed_point_precision_16_16 se2;
	se2 = 0;
	UB.m0 = LLpp.m0 - se2;
	se2 = 0;
	LB.m0 = (LLp.m0 - se2) / UB.m0;
	se2 = 0;
	se2 += LB.m2 * UB.m0;
	UB.m2 = LLpp.m2 - se2;
	se2 = 0;
	LB.m2 = (LLp.m2 - se2) / UB.m0;
	se2 = 0;
	se2 += LB.m2 * UB.m1;
	UB.m3 = LLpp.m3 - se2;
	se2 = 0;
	se2 += LB.m2 * UB.m1;
	LB.m3 = (LLp.m3 - se2) / UB.m3;



//Big_Matrix_List LU_List = {2, , PD};

//Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
Matrix LU2 = {0,0,0,0};

	LU2.m0 = LB.m0 + UB.m0 - IDT.m0;
	LU2.m3 = LB.m3 + UB.m3 - IDT.m3;

// end decomp

Matrix LTI = LU2;

Big_Vector bba0 = {N, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	bba0.m0 = (fixed_point_precision_16_16)1.0;
Big_Vector ba0 = {N, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	ba0.m0 += PB.m0 * bba0.m0;
	ba0.m0 += PB.m1 * bba0.m1;
	ba0.m1 += PB.m2 * bba0.m0;
	ba0.m1 += PB.m3 * bba0.m1;
	ba0.m1 = ba0.m1 - LU2.m2 * ba0.m0;
	ba0.m1 = ba0.m1 / LU2.m3;
	ba0.m0 = ba0.m0 - LU2.m1 * ba0.m1;
	LTI.m0 = ba0.m0;
	LTI.m2 = ba0.m1;
Big_Vector bba1 = {N, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	bba1.m1 = (fixed_point_precision_16_16)1.0;
Big_Vector ba1 = {N, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	ba1.m0 += PB.m0 * bba1.m0;
	ba1.m0 += PB.m1 * bba1.m1;
	ba1.m1 += PB.m2 * bba1.m0;
	ba1.m1 += PB.m3 * bba1.m1;
	ba1.m1 = ba1.m1 - LU2.m2 * ba1.m0;
	ba1.m1 = ba1.m1 / LU2.m3;
	ba1.m0 = ba1.m0 - LU2.m1 * ba1.m1;
	LTI.m1 = ba1.m0;
	LTI.m3 = ba1.m1;

// end inverse

Matrix LID = {0,0,0,0};


//I = mul: (L.I * D) * L.T.I
	LID.m0 = LI.m0 * D.m0;
	LID.m0 = LI.m1 * D.m2;
	LID.m1 = LI.m0 * D.m1;
	LID.m1 = LI.m1 * D.m3;
	LID.m2 = LI.m2 * D.m0;
	LID.m2 = LI.m3 * D.m2;
	LID.m3 = LI.m2 * D.m1;
	LID.m3 = LI.m3 * D.m3;


Matrix I = {0,0,0,0};


	I.m0 = LID.m0 * LTI.m0;
	I.m0 = LID.m1 * LTI.m2;
	I.m1 = LID.m0 * LTI.m1;
	I.m1 = LID.m1 * LTI.m3;
	I.m2 = LID.m2 * LTI.m0;
	I.m2 = LID.m3 * LTI.m2;
	I.m3 = LID.m2 * LTI.m1;
	I.m3 = LID.m3 * LTI.m3;


printf("I EQ");
print_mat(I);

fixed_point_precision_16_16 stop = norm_mat(I);

return stop;
}


fixed_point_precision_16_16 norm_mat(Matrix X){
  fixed_point_precision_16_16 sum = 0.0;
	sum = sum + pow_2(X.m0);
	sum = sum + pow_2(X.m1);
	sum = sum + pow_2(X.m2);
	sum = sum + pow_2(X.m3);


  fixed_point_precision_16_16 r = sqrt_val(sum);
  return r;
}



int psd(Matrix X){ // X is psd <-> Cholesky decomposable

int r = 1;

Matrix R = {0,0,0,0};


	fixed_point_precision_16_16 s;
	s = 0.0;
	fixed_point_precision_16_16 to_roota0 = X.m0 - s;
	if (to_roota0 < (fixed_point_precision_16_16)(0.0)){
	r = 0;
	}
	R.m0 = sqrt_val(to_roota0);
	s = 0.0;
	R.m2 = (fixed_point_precision_16_16)(1.0) / R.m0 * (X.m2 - s);	s = 0.0;
	s = s + R.m2 * R.m2;
	fixed_point_precision_16_16 to_roota1 = X.m3 - s;
	if (to_roota1 < (fixed_point_precision_16_16)(0.0)){
	r = 0;
	}
	R.m3 = sqrt_val(to_roota1);

//print_mat(R);
return r;
}




Solution sdp(Matrix C, Matrix Xp, Matrix_List A, Vector b, fixed_point_precision_16_16 theta, fixed_point_precision_16_16 beta){


  // Reference: http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-251j-introduction-to-mathematical-programming-fall-2009/readings/MIT6_251JF09_SDP.pdf

  int p = psd(Xp);
  Solution Q = {0,Xp,{0,0}};

	Vector new_y = {0,0};


  if (p  && (d_equal(dot(A.m0, Xp),b.m0)) && (d_equal(dot(A.m1, Xp),b.m1))){

    //beta approx
    //Q.X = beta_approx(C,Q.X,A,b,theta,beta);

    fixed_point_precision_16_16 stop = 1.0;
    fixed_point_precision_16_16 err = 0.0;

    int loop = 0;
    while((loop < 6) && !(err > epsilon) && !(stop <= (fixed_point_precision_16_16)0.25)){
      loop++;
      printf("beta X");
      print_mat(Q.X);

      Matrix L = {0,0,0,0};
 //cholesky(Q.X);

      	fixed_point_precision_16_16 s;
	s = (fixed_point_precision_16_16)0.0;
	fixed_point_precision_16_16 to_root0 = Q.X.m0 - s;
	L.m0 = sqrt_val(to_root0);
	s = (fixed_point_precision_16_16)0.0;
	L.m2 = (fixed_point_precision_16_16)1.0 / L.m0 * (Q.X.m2 - s);
	s = (fixed_point_precision_16_16)0.0;
	s = s + L.m2 * L.m2;
	fixed_point_precision_16_16 to_root1 = Q.X.m3 - s;
	L.m3 = sqrt_val(to_root1);

      //end cholesky

      printf("L");
      print_mat(L);

      //Eq_Sol sol = solve_eq(Q.X,A,C,theta);

      Matrix D = {0,0,0,0};


      //set up
      //Matrix U = mat_sub(Q.X,scal_div(mat_mul(mat_mul(Q.X,C),Q.X),theta));
      Matrix U1 = {0,0,0,0};

      	U1.m0 += Q.X.m0 * C.m0;
	U1.m0 += Q.X.m1 * C.m2;
	U1.m1 += Q.X.m0 * C.m1;
	U1.m1 += Q.X.m1 * C.m3;
	U1.m2 += Q.X.m2 * C.m0;
	U1.m2 += Q.X.m3 * C.m2;
	U1.m3 += Q.X.m2 * C.m1;
	U1.m3 += Q.X.m3 * C.m3;


      Matrix U = {0,0,0,0};

      	U.m0 += U1.m0 * Q.X.m0;
	U.m0 += U1.m1 * Q.X.m2;
	U.m1 += U1.m0 * Q.X.m1;
	U.m1 += U1.m1 * Q.X.m3;
	U.m2 += U1.m2 * Q.X.m0;
	U.m2 += U1.m3 * Q.X.m2;
	U.m3 += U1.m2 * Q.X.m1;
	U.m3 += U1.m3 * Q.X.m3;


      	U.m0 = Q.X.m0 - (U.m0 / theta);
	U.m1 = Q.X.m1 - (U.m1 / theta);
	U.m2 = Q.X.m2 - (U.m2 / theta);
	U.m3 = Q.X.m3 - (U.m3 / theta);


      Big_Matrix K = {1, N*N+M, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


      	K.m0 = U.m0;
	K.m1 = U.m1;
	K.m2 = U.m2;
	K.m3 = U.m3;



      Big_Matrix QQ = {N*N,M,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

      Matrix nX = scal_mul(Q.X,-1);

      printf("nX");
      print_mat(nX);
      /*for (int ib = 0; ib < Q.X.rows*Q.X.cols; ib++){
				nX.m[ib] = Q.X.m[ib] * -1;
      }*/
      //for(int a = 0; a < A.len; a++){
      //Matrix AQ.m0 = scal_div(mat_mul(mat_mul(nX,AQ.m0),Q.X),theta);
      	Matrix AQm0 = scal_div(mat_mul(mat_mul(nX,AQm0),Q.X),theta);
	Matrix AQm1 = scal_div(mat_mul(mat_mul(nX,AQm1),Q.X),theta);


      printf("AQm...");
      print_mat(AQm0);
      print_mat_b(AQm1);

      /*Matrix AT0 = {0,0,0,0};

      Matrix AT1 = {0,0,0,0};

      for (int vb = 0; vb < N; ++vb) {
			  for (int jb = 0; jb < N; ++jb) {
			    for (int kb = 0; kb < N; ++kb) {
						AT0.m[vb*(N)+jb] += get(vb,kb,nX) * get(kb,jb,A.m0);
						AT1.m[vb*(N)+jb] += get(vb,kb,nX) * get(kb,jb,A.m1);
			    }
			  }
      }

      Matrix AT00 = {0,0,0,0};

      Matrix AT11 = {0,0,0,0};

      for (int va = 0; va < N; ++va) {
			  for (int ja = 0; ja < N; ++ja) {
			    for (int ka = 0; ka < N; ++ka) {
						AT00.m[va*(N)+ja] += get(va,ka,AT0) * get(ka,ja,Q.X);
						AT11.m[va*(N)+ja] += get(va,ka,AT1) * get(ka,ja,Q.X);
			    }
			  }
      }

      for (int ia = 0; ia < N*N; ia++){
				AT00.m[ia] = AT00.m[ia] / theta;
				AT11.m[ia] = AT11.m[ia] / theta;
      }*/
      //AQ.m0 = flatten(AQ.m0);


      //AQ.m1 = scal_div(mat_mul(mat_mul(nX,AQ.m1),Q.X),theta);
      //AQ.m1 = flatten(AQ.m1);

      //}
      	QQ.m0 = AQm0.m0;
	QQ.m1 = AQm1.m0;
	QQ.m2 = AQm0.m1;
	QQ.m3 = AQm1.m1;
	QQ.m4 = AQm0.m2;
	QQ.m5 = AQm1.m2;
	QQ.m6 = AQm0.m3;
	QQ.m7 = AQm1.m3;




      Big_Matrix R = {M,N*N, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

      	R.m0 = AQm0.m0;
	R.m4 = AQm1.m0;
	R.m1 = AQm0.m1;
	R.m5 = AQm1.m1;
	R.m2 = AQm0.m2;
	R.m6 = AQm1.m2;
	R.m3 = AQm0.m3;
	R.m7 = AQm1.m3;


      printf("R");
      print_mat_b(R);

      Big_Matrix LL = {(N*N + M),(N*N + M), 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

        //prepend identity matrix to Q
      	LL.m0 = (fixed_point_precision_16_16)1.0;
	LL.m4 = QQ.m0;
	LL.m5 = QQ.m1;
	LL.m7 = (fixed_point_precision_16_16)1.0;
	LL.m10 = QQ.m2;
	LL.m11 = QQ.m3;
	LL.m14 = (fixed_point_precision_16_16)1.0;
	LL.m16 = QQ.m4;
	LL.m17 = QQ.m5;
	LL.m21 = (fixed_point_precision_16_16)1.0;
	LL.m22 = QQ.m6;
	LL.m23 = QQ.m7;


      printf("LL\n");

      // append P, zeros
      	LL.m28 = (fixed_point_precision_16_16)0.0;
	LL.m29 = (fixed_point_precision_16_16)0.0;
	LL.m34 = (fixed_point_precision_16_16)0.0;
	LL.m35 = (fixed_point_precision_16_16)0.0;



      //least sq solution
      Big_Vector e = vectorize(K);
      	e.m0 = K.m0;
	e.m1 = K.m1;
	e.m2 = K.m2;
	e.m3 = K.m3;
	e.m4 = K.m4;
	e.m5 = K.m5;


      //Big_Vector z = LUP_solve(LL, e);
       //Big_Matrix_List LU_List = LUP_decompose_big(LL);
       //printf("REAL LU");
       //print_mat_b(LU_List.m0);


      Big_Matrix LD = {(N*N + M),(N*N + M), 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

      Big_Matrix UD = {(N*N + M),(N*N + M), 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

      Big_Matrix PD = {(N*N + M),(N*N + M), 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


      printf("pre piv\n");
      //pivot A P
      	PD.m0 = (fixed_point_precision_16_16)1.0;
	PD.m7 = (fixed_point_precision_16_16)1.0;
	PD.m14 = (fixed_point_precision_16_16)1.0;
	PD.m21 = (fixed_point_precision_16_16)1.0;
	PD.m28 = (fixed_point_precision_16_16)1.0;
	PD.m35 = (fixed_point_precision_16_16)1.0;


      	fixed_point_precision_16_16 cmp3; int max_j3;
	max_j3 = 0;
	if (abs_val(LL.m0) > abs_val(cmp3)){ max_j3 = 0; }
	if (abs_val(LL.m6) > abs_val(cmp3)){ max_j3 = 1; }
	if (abs_val(LL.m12) > abs_val(cmp3)){ max_j3 = 2; }
	if (abs_val(LL.m18) > abs_val(cmp3)){ max_j3 = 3; }
	if (abs_val(LL.m24) > abs_val(cmp3)){ max_j3 = 4; }
	if (abs_val(LL.m30) > abs_val(cmp3)){ max_j3 = 5; }
if (max_j3 == 0){}
if (max_j3 == 1){	fixed_point_precision_16_16 temp30;
	temp30 = PD.m0;
	PD.m0 = PD.m6;
	PD.m6 = temp30;
	temp30 = PD.m1;
	PD.m1 = PD.m7;
	PD.m7 = temp30;
	temp30 = PD.m2;
	PD.m2 = PD.m8;
	PD.m8 = temp30;
	temp30 = PD.m3;
	PD.m3 = PD.m9;
	PD.m9 = temp30;
	temp30 = PD.m4;
	PD.m4 = PD.m10;
	PD.m10 = temp30;
	temp30 = PD.m5;
	PD.m5 = PD.m11;
	PD.m11 = temp30;
}
if (max_j3 == 2){	fixed_point_precision_16_16 temp30;
	temp30 = PD.m0;
	PD.m0 = PD.m12;
	PD.m12 = temp30;
	temp30 = PD.m1;
	PD.m1 = PD.m13;
	PD.m13 = temp30;
	temp30 = PD.m2;
	PD.m2 = PD.m14;
	PD.m14 = temp30;
	temp30 = PD.m3;
	PD.m3 = PD.m15;
	PD.m15 = temp30;
	temp30 = PD.m4;
	PD.m4 = PD.m16;
	PD.m16 = temp30;
	temp30 = PD.m5;
	PD.m5 = PD.m17;
	PD.m17 = temp30;
}
if (max_j3 == 3){	fixed_point_precision_16_16 temp30;
	temp30 = PD.m0;
	PD.m0 = PD.m18;
	PD.m18 = temp30;
	temp30 = PD.m1;
	PD.m1 = PD.m19;
	PD.m19 = temp30;
	temp30 = PD.m2;
	PD.m2 = PD.m20;
	PD.m20 = temp30;
	temp30 = PD.m3;
	PD.m3 = PD.m21;
	PD.m21 = temp30;
	temp30 = PD.m4;
	PD.m4 = PD.m22;
	PD.m22 = temp30;
	temp30 = PD.m5;
	PD.m5 = PD.m23;
	PD.m23 = temp30;
}
if (max_j3 == 4){	fixed_point_precision_16_16 temp30;
	temp30 = PD.m0;
	PD.m0 = PD.m24;
	PD.m24 = temp30;
	temp30 = PD.m1;
	PD.m1 = PD.m25;
	PD.m25 = temp30;
	temp30 = PD.m2;
	PD.m2 = PD.m26;
	PD.m26 = temp30;
	temp30 = PD.m3;
	PD.m3 = PD.m27;
	PD.m27 = temp30;
	temp30 = PD.m4;
	PD.m4 = PD.m28;
	PD.m28 = temp30;
	temp30 = PD.m5;
	PD.m5 = PD.m29;
	PD.m29 = temp30;
}
if (max_j3 == 5){	fixed_point_precision_16_16 temp30;
	temp30 = PD.m0;
	PD.m0 = PD.m30;
	PD.m30 = temp30;
	temp30 = PD.m1;
	PD.m1 = PD.m31;
	PD.m31 = temp30;
	temp30 = PD.m2;
	PD.m2 = PD.m32;
	PD.m32 = temp30;
	temp30 = PD.m3;
	PD.m3 = PD.m33;
	PD.m33 = temp30;
	temp30 = PD.m4;
	PD.m4 = PD.m34;
	PD.m34 = temp30;
	temp30 = PD.m5;
	PD.m5 = PD.m35;
	PD.m35 = temp30;
}
	max_j3 = 1;
	if (abs_val(LL.m1) > abs_val(cmp3)){ max_j3 = 0; }
	if (abs_val(LL.m7) > abs_val(cmp3)){ max_j3 = 1; }
	if (abs_val(LL.m13) > abs_val(cmp3)){ max_j3 = 2; }
	if (abs_val(LL.m19) > abs_val(cmp3)){ max_j3 = 3; }
	if (abs_val(LL.m25) > abs_val(cmp3)){ max_j3 = 4; }
	if (abs_val(LL.m31) > abs_val(cmp3)){ max_j3 = 5; }
if (max_j3 == 0){	fixed_point_precision_16_16 temp31;
	temp31 = PD.m6;
	PD.m6 = PD.m0;
	PD.m0 = temp31;
	temp31 = PD.m7;
	PD.m7 = PD.m1;
	PD.m1 = temp31;
	temp31 = PD.m8;
	PD.m8 = PD.m2;
	PD.m2 = temp31;
	temp31 = PD.m9;
	PD.m9 = PD.m3;
	PD.m3 = temp31;
	temp31 = PD.m10;
	PD.m10 = PD.m4;
	PD.m4 = temp31;
	temp31 = PD.m11;
	PD.m11 = PD.m5;
	PD.m5 = temp31;
}
if (max_j3 == 1){}
if (max_j3 == 2){	fixed_point_precision_16_16 temp31;
	temp31 = PD.m6;
	PD.m6 = PD.m12;
	PD.m12 = temp31;
	temp31 = PD.m7;
	PD.m7 = PD.m13;
	PD.m13 = temp31;
	temp31 = PD.m8;
	PD.m8 = PD.m14;
	PD.m14 = temp31;
	temp31 = PD.m9;
	PD.m9 = PD.m15;
	PD.m15 = temp31;
	temp31 = PD.m10;
	PD.m10 = PD.m16;
	PD.m16 = temp31;
	temp31 = PD.m11;
	PD.m11 = PD.m17;
	PD.m17 = temp31;
}
if (max_j3 == 3){	fixed_point_precision_16_16 temp31;
	temp31 = PD.m6;
	PD.m6 = PD.m18;
	PD.m18 = temp31;
	temp31 = PD.m7;
	PD.m7 = PD.m19;
	PD.m19 = temp31;
	temp31 = PD.m8;
	PD.m8 = PD.m20;
	PD.m20 = temp31;
	temp31 = PD.m9;
	PD.m9 = PD.m21;
	PD.m21 = temp31;
	temp31 = PD.m10;
	PD.m10 = PD.m22;
	PD.m22 = temp31;
	temp31 = PD.m11;
	PD.m11 = PD.m23;
	PD.m23 = temp31;
}
if (max_j3 == 4){	fixed_point_precision_16_16 temp31;
	temp31 = PD.m6;
	PD.m6 = PD.m24;
	PD.m24 = temp31;
	temp31 = PD.m7;
	PD.m7 = PD.m25;
	PD.m25 = temp31;
	temp31 = PD.m8;
	PD.m8 = PD.m26;
	PD.m26 = temp31;
	temp31 = PD.m9;
	PD.m9 = PD.m27;
	PD.m27 = temp31;
	temp31 = PD.m10;
	PD.m10 = PD.m28;
	PD.m28 = temp31;
	temp31 = PD.m11;
	PD.m11 = PD.m29;
	PD.m29 = temp31;
}
if (max_j3 == 5){	fixed_point_precision_16_16 temp31;
	temp31 = PD.m6;
	PD.m6 = PD.m30;
	PD.m30 = temp31;
	temp31 = PD.m7;
	PD.m7 = PD.m31;
	PD.m31 = temp31;
	temp31 = PD.m8;
	PD.m8 = PD.m32;
	PD.m32 = temp31;
	temp31 = PD.m9;
	PD.m9 = PD.m33;
	PD.m33 = temp31;
	temp31 = PD.m10;
	PD.m10 = PD.m34;
	PD.m34 = temp31;
	temp31 = PD.m11;
	PD.m11 = PD.m35;
	PD.m35 = temp31;
}
	max_j3 = 2;
	if (abs_val(LL.m2) > abs_val(cmp3)){ max_j3 = 0; }
	if (abs_val(LL.m8) > abs_val(cmp3)){ max_j3 = 1; }
	if (abs_val(LL.m14) > abs_val(cmp3)){ max_j3 = 2; }
	if (abs_val(LL.m20) > abs_val(cmp3)){ max_j3 = 3; }
	if (abs_val(LL.m26) > abs_val(cmp3)){ max_j3 = 4; }
	if (abs_val(LL.m32) > abs_val(cmp3)){ max_j3 = 5; }
if (max_j3 == 0){	fixed_point_precision_16_16 temp32;
	temp32 = PD.m12;
	PD.m12 = PD.m0;
	PD.m0 = temp32;
	temp32 = PD.m13;
	PD.m13 = PD.m1;
	PD.m1 = temp32;
	temp32 = PD.m14;
	PD.m14 = PD.m2;
	PD.m2 = temp32;
	temp32 = PD.m15;
	PD.m15 = PD.m3;
	PD.m3 = temp32;
	temp32 = PD.m16;
	PD.m16 = PD.m4;
	PD.m4 = temp32;
	temp32 = PD.m17;
	PD.m17 = PD.m5;
	PD.m5 = temp32;
}
if (max_j3 == 1){	fixed_point_precision_16_16 temp32;
	temp32 = PD.m12;
	PD.m12 = PD.m6;
	PD.m6 = temp32;
	temp32 = PD.m13;
	PD.m13 = PD.m7;
	PD.m7 = temp32;
	temp32 = PD.m14;
	PD.m14 = PD.m8;
	PD.m8 = temp32;
	temp32 = PD.m15;
	PD.m15 = PD.m9;
	PD.m9 = temp32;
	temp32 = PD.m16;
	PD.m16 = PD.m10;
	PD.m10 = temp32;
	temp32 = PD.m17;
	PD.m17 = PD.m11;
	PD.m11 = temp32;
}
if (max_j3 == 2){}
if (max_j3 == 3){	fixed_point_precision_16_16 temp32;
	temp32 = PD.m12;
	PD.m12 = PD.m18;
	PD.m18 = temp32;
	temp32 = PD.m13;
	PD.m13 = PD.m19;
	PD.m19 = temp32;
	temp32 = PD.m14;
	PD.m14 = PD.m20;
	PD.m20 = temp32;
	temp32 = PD.m15;
	PD.m15 = PD.m21;
	PD.m21 = temp32;
	temp32 = PD.m16;
	PD.m16 = PD.m22;
	PD.m22 = temp32;
	temp32 = PD.m17;
	PD.m17 = PD.m23;
	PD.m23 = temp32;
}
if (max_j3 == 4){	fixed_point_precision_16_16 temp32;
	temp32 = PD.m12;
	PD.m12 = PD.m24;
	PD.m24 = temp32;
	temp32 = PD.m13;
	PD.m13 = PD.m25;
	PD.m25 = temp32;
	temp32 = PD.m14;
	PD.m14 = PD.m26;
	PD.m26 = temp32;
	temp32 = PD.m15;
	PD.m15 = PD.m27;
	PD.m27 = temp32;
	temp32 = PD.m16;
	PD.m16 = PD.m28;
	PD.m28 = temp32;
	temp32 = PD.m17;
	PD.m17 = PD.m29;
	PD.m29 = temp32;
}
if (max_j3 == 5){	fixed_point_precision_16_16 temp32;
	temp32 = PD.m12;
	PD.m12 = PD.m30;
	PD.m30 = temp32;
	temp32 = PD.m13;
	PD.m13 = PD.m31;
	PD.m31 = temp32;
	temp32 = PD.m14;
	PD.m14 = PD.m32;
	PD.m32 = temp32;
	temp32 = PD.m15;
	PD.m15 = PD.m33;
	PD.m33 = temp32;
	temp32 = PD.m16;
	PD.m16 = PD.m34;
	PD.m34 = temp32;
	temp32 = PD.m17;
	PD.m17 = PD.m35;
	PD.m35 = temp32;
}
	max_j3 = 3;
	if (abs_val(LL.m3) > abs_val(cmp3)){ max_j3 = 0; }
	if (abs_val(LL.m9) > abs_val(cmp3)){ max_j3 = 1; }
	if (abs_val(LL.m15) > abs_val(cmp3)){ max_j3 = 2; }
	if (abs_val(LL.m21) > abs_val(cmp3)){ max_j3 = 3; }
	if (abs_val(LL.m27) > abs_val(cmp3)){ max_j3 = 4; }
	if (abs_val(LL.m33) > abs_val(cmp3)){ max_j3 = 5; }
if (max_j3 == 0){	fixed_point_precision_16_16 temp33;
	temp33 = PD.m18;
	PD.m18 = PD.m0;
	PD.m0 = temp33;
	temp33 = PD.m19;
	PD.m19 = PD.m1;
	PD.m1 = temp33;
	temp33 = PD.m20;
	PD.m20 = PD.m2;
	PD.m2 = temp33;
	temp33 = PD.m21;
	PD.m21 = PD.m3;
	PD.m3 = temp33;
	temp33 = PD.m22;
	PD.m22 = PD.m4;
	PD.m4 = temp33;
	temp33 = PD.m23;
	PD.m23 = PD.m5;
	PD.m5 = temp33;
}
if (max_j3 == 1){	fixed_point_precision_16_16 temp33;
	temp33 = PD.m18;
	PD.m18 = PD.m6;
	PD.m6 = temp33;
	temp33 = PD.m19;
	PD.m19 = PD.m7;
	PD.m7 = temp33;
	temp33 = PD.m20;
	PD.m20 = PD.m8;
	PD.m8 = temp33;
	temp33 = PD.m21;
	PD.m21 = PD.m9;
	PD.m9 = temp33;
	temp33 = PD.m22;
	PD.m22 = PD.m10;
	PD.m10 = temp33;
	temp33 = PD.m23;
	PD.m23 = PD.m11;
	PD.m11 = temp33;
}
if (max_j3 == 2){	fixed_point_precision_16_16 temp33;
	temp33 = PD.m18;
	PD.m18 = PD.m12;
	PD.m12 = temp33;
	temp33 = PD.m19;
	PD.m19 = PD.m13;
	PD.m13 = temp33;
	temp33 = PD.m20;
	PD.m20 = PD.m14;
	PD.m14 = temp33;
	temp33 = PD.m21;
	PD.m21 = PD.m15;
	PD.m15 = temp33;
	temp33 = PD.m22;
	PD.m22 = PD.m16;
	PD.m16 = temp33;
	temp33 = PD.m23;
	PD.m23 = PD.m17;
	PD.m17 = temp33;
}
if (max_j3 == 3){}
if (max_j3 == 4){	fixed_point_precision_16_16 temp33;
	temp33 = PD.m18;
	PD.m18 = PD.m24;
	PD.m24 = temp33;
	temp33 = PD.m19;
	PD.m19 = PD.m25;
	PD.m25 = temp33;
	temp33 = PD.m20;
	PD.m20 = PD.m26;
	PD.m26 = temp33;
	temp33 = PD.m21;
	PD.m21 = PD.m27;
	PD.m27 = temp33;
	temp33 = PD.m22;
	PD.m22 = PD.m28;
	PD.m28 = temp33;
	temp33 = PD.m23;
	PD.m23 = PD.m29;
	PD.m29 = temp33;
}
if (max_j3 == 5){	fixed_point_precision_16_16 temp33;
	temp33 = PD.m18;
	PD.m18 = PD.m30;
	PD.m30 = temp33;
	temp33 = PD.m19;
	PD.m19 = PD.m31;
	PD.m31 = temp33;
	temp33 = PD.m20;
	PD.m20 = PD.m32;
	PD.m32 = temp33;
	temp33 = PD.m21;
	PD.m21 = PD.m33;
	PD.m33 = temp33;
	temp33 = PD.m22;
	PD.m22 = PD.m34;
	PD.m34 = temp33;
	temp33 = PD.m23;
	PD.m23 = PD.m35;
	PD.m35 = temp33;
}
	max_j3 = 4;
	if (abs_val(LL.m4) > abs_val(cmp3)){ max_j3 = 0; }
	if (abs_val(LL.m10) > abs_val(cmp3)){ max_j3 = 1; }
	if (abs_val(LL.m16) > abs_val(cmp3)){ max_j3 = 2; }
	if (abs_val(LL.m22) > abs_val(cmp3)){ max_j3 = 3; }
	if (abs_val(LL.m28) > abs_val(cmp3)){ max_j3 = 4; }
	if (abs_val(LL.m34) > abs_val(cmp3)){ max_j3 = 5; }
if (max_j3 == 0){	fixed_point_precision_16_16 temp34;
	temp34 = PD.m24;
	PD.m24 = PD.m0;
	PD.m0 = temp34;
	temp34 = PD.m25;
	PD.m25 = PD.m1;
	PD.m1 = temp34;
	temp34 = PD.m26;
	PD.m26 = PD.m2;
	PD.m2 = temp34;
	temp34 = PD.m27;
	PD.m27 = PD.m3;
	PD.m3 = temp34;
	temp34 = PD.m28;
	PD.m28 = PD.m4;
	PD.m4 = temp34;
	temp34 = PD.m29;
	PD.m29 = PD.m5;
	PD.m5 = temp34;
}
if (max_j3 == 1){	fixed_point_precision_16_16 temp34;
	temp34 = PD.m24;
	PD.m24 = PD.m6;
	PD.m6 = temp34;
	temp34 = PD.m25;
	PD.m25 = PD.m7;
	PD.m7 = temp34;
	temp34 = PD.m26;
	PD.m26 = PD.m8;
	PD.m8 = temp34;
	temp34 = PD.m27;
	PD.m27 = PD.m9;
	PD.m9 = temp34;
	temp34 = PD.m28;
	PD.m28 = PD.m10;
	PD.m10 = temp34;
	temp34 = PD.m29;
	PD.m29 = PD.m11;
	PD.m11 = temp34;
}
if (max_j3 == 2){	fixed_point_precision_16_16 temp34;
	temp34 = PD.m24;
	PD.m24 = PD.m12;
	PD.m12 = temp34;
	temp34 = PD.m25;
	PD.m25 = PD.m13;
	PD.m13 = temp34;
	temp34 = PD.m26;
	PD.m26 = PD.m14;
	PD.m14 = temp34;
	temp34 = PD.m27;
	PD.m27 = PD.m15;
	PD.m15 = temp34;
	temp34 = PD.m28;
	PD.m28 = PD.m16;
	PD.m16 = temp34;
	temp34 = PD.m29;
	PD.m29 = PD.m17;
	PD.m17 = temp34;
}
if (max_j3 == 3){	fixed_point_precision_16_16 temp34;
	temp34 = PD.m24;
	PD.m24 = PD.m18;
	PD.m18 = temp34;
	temp34 = PD.m25;
	PD.m25 = PD.m19;
	PD.m19 = temp34;
	temp34 = PD.m26;
	PD.m26 = PD.m20;
	PD.m20 = temp34;
	temp34 = PD.m27;
	PD.m27 = PD.m21;
	PD.m21 = temp34;
	temp34 = PD.m28;
	PD.m28 = PD.m22;
	PD.m22 = temp34;
	temp34 = PD.m29;
	PD.m29 = PD.m23;
	PD.m23 = temp34;
}
if (max_j3 == 4){}
if (max_j3 == 5){	fixed_point_precision_16_16 temp34;
	temp34 = PD.m24;
	PD.m24 = PD.m30;
	PD.m30 = temp34;
	temp34 = PD.m25;
	PD.m25 = PD.m31;
	PD.m31 = temp34;
	temp34 = PD.m26;
	PD.m26 = PD.m32;
	PD.m32 = temp34;
	temp34 = PD.m27;
	PD.m27 = PD.m33;
	PD.m33 = temp34;
	temp34 = PD.m28;
	PD.m28 = PD.m34;
	PD.m34 = temp34;
	temp34 = PD.m29;
	PD.m29 = PD.m35;
	PD.m35 = temp34;
}
	max_j3 = 5;
	if (abs_val(LL.m5) > abs_val(cmp3)){ max_j3 = 0; }
	if (abs_val(LL.m11) > abs_val(cmp3)){ max_j3 = 1; }
	if (abs_val(LL.m17) > abs_val(cmp3)){ max_j3 = 2; }
	if (abs_val(LL.m23) > abs_val(cmp3)){ max_j3 = 3; }
	if (abs_val(LL.m29) > abs_val(cmp3)){ max_j3 = 4; }
	if (abs_val(LL.m35) > abs_val(cmp3)){ max_j3 = 5; }
if (max_j3 == 0){	fixed_point_precision_16_16 temp35;
	temp35 = PD.m30;
	PD.m30 = PD.m0;
	PD.m0 = temp35;
	temp35 = PD.m31;
	PD.m31 = PD.m1;
	PD.m1 = temp35;
	temp35 = PD.m32;
	PD.m32 = PD.m2;
	PD.m2 = temp35;
	temp35 = PD.m33;
	PD.m33 = PD.m3;
	PD.m3 = temp35;
	temp35 = PD.m34;
	PD.m34 = PD.m4;
	PD.m4 = temp35;
	temp35 = PD.m35;
	PD.m35 = PD.m5;
	PD.m5 = temp35;
}
if (max_j3 == 1){	fixed_point_precision_16_16 temp35;
	temp35 = PD.m30;
	PD.m30 = PD.m6;
	PD.m6 = temp35;
	temp35 = PD.m31;
	PD.m31 = PD.m7;
	PD.m7 = temp35;
	temp35 = PD.m32;
	PD.m32 = PD.m8;
	PD.m8 = temp35;
	temp35 = PD.m33;
	PD.m33 = PD.m9;
	PD.m9 = temp35;
	temp35 = PD.m34;
	PD.m34 = PD.m10;
	PD.m10 = temp35;
	temp35 = PD.m35;
	PD.m35 = PD.m11;
	PD.m11 = temp35;
}
if (max_j3 == 2){	fixed_point_precision_16_16 temp35;
	temp35 = PD.m30;
	PD.m30 = PD.m12;
	PD.m12 = temp35;
	temp35 = PD.m31;
	PD.m31 = PD.m13;
	PD.m13 = temp35;
	temp35 = PD.m32;
	PD.m32 = PD.m14;
	PD.m14 = temp35;
	temp35 = PD.m33;
	PD.m33 = PD.m15;
	PD.m15 = temp35;
	temp35 = PD.m34;
	PD.m34 = PD.m16;
	PD.m16 = temp35;
	temp35 = PD.m35;
	PD.m35 = PD.m17;
	PD.m17 = temp35;
}
if (max_j3 == 3){	fixed_point_precision_16_16 temp35;
	temp35 = PD.m30;
	PD.m30 = PD.m18;
	PD.m18 = temp35;
	temp35 = PD.m31;
	PD.m31 = PD.m19;
	PD.m19 = temp35;
	temp35 = PD.m32;
	PD.m32 = PD.m20;
	PD.m20 = temp35;
	temp35 = PD.m33;
	PD.m33 = PD.m21;
	PD.m21 = temp35;
	temp35 = PD.m34;
	PD.m34 = PD.m22;
	PD.m22 = temp35;
	temp35 = PD.m35;
	PD.m35 = PD.m23;
	PD.m23 = temp35;
}
if (max_j3 == 4){	fixed_point_precision_16_16 temp35;
	temp35 = PD.m30;
	PD.m30 = PD.m24;
	PD.m24 = temp35;
	temp35 = PD.m31;
	PD.m31 = PD.m25;
	PD.m25 = temp35;
	temp35 = PD.m32;
	PD.m32 = PD.m26;
	PD.m26 = temp35;
	temp35 = PD.m33;
	PD.m33 = PD.m27;
	PD.m27 = temp35;
	temp35 = PD.m34;
	PD.m34 = PD.m28;
	PD.m28 = temp35;
	temp35 = PD.m35;
	PD.m35 = PD.m29;
	PD.m29 = temp35;
}
if (max_j3 == 5){}


      printf("post piv/swap\n");

      //Big_Matrix LLp = mat_mul_big(PD,LL);
      Big_Matrix LLp = {(N*N + M),(N*N + M), 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


      	LLp.m0 = PD.m0 * LL.m0;
	LLp.m0 = PD.m1 * LL.m6;
	LLp.m0 = PD.m2 * LL.m12;
	LLp.m0 = PD.m3 * LL.m18;
	LLp.m0 = PD.m4 * LL.m24;
	LLp.m0 = PD.m5 * LL.m30;
	LLp.m1 = PD.m0 * LL.m1;
	LLp.m1 = PD.m1 * LL.m7;
	LLp.m1 = PD.m2 * LL.m13;
	LLp.m1 = PD.m3 * LL.m19;
	LLp.m1 = PD.m4 * LL.m25;
	LLp.m1 = PD.m5 * LL.m31;
	LLp.m2 = PD.m0 * LL.m2;
	LLp.m2 = PD.m1 * LL.m8;
	LLp.m2 = PD.m2 * LL.m14;
	LLp.m2 = PD.m3 * LL.m20;
	LLp.m2 = PD.m4 * LL.m26;
	LLp.m2 = PD.m5 * LL.m32;
	LLp.m3 = PD.m0 * LL.m3;
	LLp.m3 = PD.m1 * LL.m9;
	LLp.m3 = PD.m2 * LL.m15;
	LLp.m3 = PD.m3 * LL.m21;
	LLp.m3 = PD.m4 * LL.m27;
	LLp.m3 = PD.m5 * LL.m33;
	LLp.m4 = PD.m0 * LL.m4;
	LLp.m4 = PD.m1 * LL.m10;
	LLp.m4 = PD.m2 * LL.m16;
	LLp.m4 = PD.m3 * LL.m22;
	LLp.m4 = PD.m4 * LL.m28;
	LLp.m4 = PD.m5 * LL.m34;
	LLp.m5 = PD.m0 * LL.m5;
	LLp.m5 = PD.m1 * LL.m11;
	LLp.m5 = PD.m2 * LL.m17;
	LLp.m5 = PD.m3 * LL.m23;
	LLp.m5 = PD.m4 * LL.m29;
	LLp.m5 = PD.m5 * LL.m35;
	LLp.m6 = PD.m6 * LL.m0;
	LLp.m6 = PD.m7 * LL.m6;
	LLp.m6 = PD.m8 * LL.m12;
	LLp.m6 = PD.m9 * LL.m18;
	LLp.m6 = PD.m10 * LL.m24;
	LLp.m6 = PD.m11 * LL.m30;
	LLp.m7 = PD.m6 * LL.m1;
	LLp.m7 = PD.m7 * LL.m7;
	LLp.m7 = PD.m8 * LL.m13;
	LLp.m7 = PD.m9 * LL.m19;
	LLp.m7 = PD.m10 * LL.m25;
	LLp.m7 = PD.m11 * LL.m31;
	LLp.m8 = PD.m6 * LL.m2;
	LLp.m8 = PD.m7 * LL.m8;
	LLp.m8 = PD.m8 * LL.m14;
	LLp.m8 = PD.m9 * LL.m20;
	LLp.m8 = PD.m10 * LL.m26;
	LLp.m8 = PD.m11 * LL.m32;
	LLp.m9 = PD.m6 * LL.m3;
	LLp.m9 = PD.m7 * LL.m9;
	LLp.m9 = PD.m8 * LL.m15;
	LLp.m9 = PD.m9 * LL.m21;
	LLp.m9 = PD.m10 * LL.m27;
	LLp.m9 = PD.m11 * LL.m33;
	LLp.m10 = PD.m6 * LL.m4;
	LLp.m10 = PD.m7 * LL.m10;
	LLp.m10 = PD.m8 * LL.m16;
	LLp.m10 = PD.m9 * LL.m22;
	LLp.m10 = PD.m10 * LL.m28;
	LLp.m10 = PD.m11 * LL.m34;
	LLp.m11 = PD.m6 * LL.m5;
	LLp.m11 = PD.m7 * LL.m11;
	LLp.m11 = PD.m8 * LL.m17;
	LLp.m11 = PD.m9 * LL.m23;
	LLp.m11 = PD.m10 * LL.m29;
	LLp.m11 = PD.m11 * LL.m35;
	LLp.m12 = PD.m12 * LL.m0;
	LLp.m12 = PD.m13 * LL.m6;
	LLp.m12 = PD.m14 * LL.m12;
	LLp.m12 = PD.m15 * LL.m18;
	LLp.m12 = PD.m16 * LL.m24;
	LLp.m12 = PD.m17 * LL.m30;
	LLp.m13 = PD.m12 * LL.m1;
	LLp.m13 = PD.m13 * LL.m7;
	LLp.m13 = PD.m14 * LL.m13;
	LLp.m13 = PD.m15 * LL.m19;
	LLp.m13 = PD.m16 * LL.m25;
	LLp.m13 = PD.m17 * LL.m31;
	LLp.m14 = PD.m12 * LL.m2;
	LLp.m14 = PD.m13 * LL.m8;
	LLp.m14 = PD.m14 * LL.m14;
	LLp.m14 = PD.m15 * LL.m20;
	LLp.m14 = PD.m16 * LL.m26;
	LLp.m14 = PD.m17 * LL.m32;
	LLp.m15 = PD.m12 * LL.m3;
	LLp.m15 = PD.m13 * LL.m9;
	LLp.m15 = PD.m14 * LL.m15;
	LLp.m15 = PD.m15 * LL.m21;
	LLp.m15 = PD.m16 * LL.m27;
	LLp.m15 = PD.m17 * LL.m33;
	LLp.m16 = PD.m12 * LL.m4;
	LLp.m16 = PD.m13 * LL.m10;
	LLp.m16 = PD.m14 * LL.m16;
	LLp.m16 = PD.m15 * LL.m22;
	LLp.m16 = PD.m16 * LL.m28;
	LLp.m16 = PD.m17 * LL.m34;
	LLp.m17 = PD.m12 * LL.m5;
	LLp.m17 = PD.m13 * LL.m11;
	LLp.m17 = PD.m14 * LL.m17;
	LLp.m17 = PD.m15 * LL.m23;
	LLp.m17 = PD.m16 * LL.m29;
	LLp.m17 = PD.m17 * LL.m35;
	LLp.m18 = PD.m18 * LL.m0;
	LLp.m18 = PD.m19 * LL.m6;
	LLp.m18 = PD.m20 * LL.m12;
	LLp.m18 = PD.m21 * LL.m18;
	LLp.m18 = PD.m22 * LL.m24;
	LLp.m18 = PD.m23 * LL.m30;
	LLp.m19 = PD.m18 * LL.m1;
	LLp.m19 = PD.m19 * LL.m7;
	LLp.m19 = PD.m20 * LL.m13;
	LLp.m19 = PD.m21 * LL.m19;
	LLp.m19 = PD.m22 * LL.m25;
	LLp.m19 = PD.m23 * LL.m31;
	LLp.m20 = PD.m18 * LL.m2;
	LLp.m20 = PD.m19 * LL.m8;
	LLp.m20 = PD.m20 * LL.m14;
	LLp.m20 = PD.m21 * LL.m20;
	LLp.m20 = PD.m22 * LL.m26;
	LLp.m20 = PD.m23 * LL.m32;
	LLp.m21 = PD.m18 * LL.m3;
	LLp.m21 = PD.m19 * LL.m9;
	LLp.m21 = PD.m20 * LL.m15;
	LLp.m21 = PD.m21 * LL.m21;
	LLp.m21 = PD.m22 * LL.m27;
	LLp.m21 = PD.m23 * LL.m33;
	LLp.m22 = PD.m18 * LL.m4;
	LLp.m22 = PD.m19 * LL.m10;
	LLp.m22 = PD.m20 * LL.m16;
	LLp.m22 = PD.m21 * LL.m22;
	LLp.m22 = PD.m22 * LL.m28;
	LLp.m22 = PD.m23 * LL.m34;
	LLp.m23 = PD.m18 * LL.m5;
	LLp.m23 = PD.m19 * LL.m11;
	LLp.m23 = PD.m20 * LL.m17;
	LLp.m23 = PD.m21 * LL.m23;
	LLp.m23 = PD.m22 * LL.m29;
	LLp.m23 = PD.m23 * LL.m35;
	LLp.m24 = PD.m24 * LL.m0;
	LLp.m24 = PD.m25 * LL.m6;
	LLp.m24 = PD.m26 * LL.m12;
	LLp.m24 = PD.m27 * LL.m18;
	LLp.m24 = PD.m28 * LL.m24;
	LLp.m24 = PD.m29 * LL.m30;
	LLp.m25 = PD.m24 * LL.m1;
	LLp.m25 = PD.m25 * LL.m7;
	LLp.m25 = PD.m26 * LL.m13;
	LLp.m25 = PD.m27 * LL.m19;
	LLp.m25 = PD.m28 * LL.m25;
	LLp.m25 = PD.m29 * LL.m31;
	LLp.m26 = PD.m24 * LL.m2;
	LLp.m26 = PD.m25 * LL.m8;
	LLp.m26 = PD.m26 * LL.m14;
	LLp.m26 = PD.m27 * LL.m20;
	LLp.m26 = PD.m28 * LL.m26;
	LLp.m26 = PD.m29 * LL.m32;
	LLp.m27 = PD.m24 * LL.m3;
	LLp.m27 = PD.m25 * LL.m9;
	LLp.m27 = PD.m26 * LL.m15;
	LLp.m27 = PD.m27 * LL.m21;
	LLp.m27 = PD.m28 * LL.m27;
	LLp.m27 = PD.m29 * LL.m33;
	LLp.m28 = PD.m24 * LL.m4;
	LLp.m28 = PD.m25 * LL.m10;
	LLp.m28 = PD.m26 * LL.m16;
	LLp.m28 = PD.m27 * LL.m22;
	LLp.m28 = PD.m28 * LL.m28;
	LLp.m28 = PD.m29 * LL.m34;
	LLp.m29 = PD.m24 * LL.m5;
	LLp.m29 = PD.m25 * LL.m11;
	LLp.m29 = PD.m26 * LL.m17;
	LLp.m29 = PD.m27 * LL.m23;
	LLp.m29 = PD.m28 * LL.m29;
	LLp.m29 = PD.m29 * LL.m35;
	LLp.m30 = PD.m30 * LL.m0;
	LLp.m30 = PD.m31 * LL.m6;
	LLp.m30 = PD.m32 * LL.m12;
	LLp.m30 = PD.m33 * LL.m18;
	LLp.m30 = PD.m34 * LL.m24;
	LLp.m30 = PD.m35 * LL.m30;
	LLp.m31 = PD.m30 * LL.m1;
	LLp.m31 = PD.m31 * LL.m7;
	LLp.m31 = PD.m32 * LL.m13;
	LLp.m31 = PD.m33 * LL.m19;
	LLp.m31 = PD.m34 * LL.m25;
	LLp.m31 = PD.m35 * LL.m31;
	LLp.m32 = PD.m30 * LL.m2;
	LLp.m32 = PD.m31 * LL.m8;
	LLp.m32 = PD.m32 * LL.m14;
	LLp.m32 = PD.m33 * LL.m20;
	LLp.m32 = PD.m34 * LL.m26;
	LLp.m32 = PD.m35 * LL.m32;
	LLp.m33 = PD.m30 * LL.m3;
	LLp.m33 = PD.m31 * LL.m9;
	LLp.m33 = PD.m32 * LL.m15;
	LLp.m33 = PD.m33 * LL.m21;
	LLp.m33 = PD.m34 * LL.m27;
	LLp.m33 = PD.m35 * LL.m33;
	LLp.m34 = PD.m30 * LL.m4;
	LLp.m34 = PD.m31 * LL.m10;
	LLp.m34 = PD.m32 * LL.m16;
	LLp.m34 = PD.m33 * LL.m22;
	LLp.m34 = PD.m34 * LL.m28;
	LLp.m34 = PD.m35 * LL.m34;
	LLp.m35 = PD.m30 * LL.m5;
	LLp.m35 = PD.m31 * LL.m11;
	LLp.m35 = PD.m32 * LL.m17;
	LLp.m35 = PD.m33 * LL.m23;
	LLp.m35 = PD.m34 * LL.m29;
	LLp.m35 = PD.m35 * LL.m35;


      //printf("MY AP");
      //print_mat_b(LLp);

      	LD.m0 = (fixed_point_precision_16_16)1.0;
	LD.m7 = (fixed_point_precision_16_16)1.0;
	LD.m14 = (fixed_point_precision_16_16)1.0;
	LD.m21 = (fixed_point_precision_16_16)1.0;
	LD.m28 = (fixed_point_precision_16_16)1.0;
	LD.m35 = (fixed_point_precision_16_16)1.0;


      //printf("MY PRE L");
      //print_mat_b(LD);



      	fixed_point_precision_16_16 se;
	se = 0;
	UD.m0 = LLp.m0 - se;
	se = 0;
	LD.m0 = (LLp.m0 - se) / UD.m0;
	se = 0;
	se += LD.m6 * UD.m0;
	UD.m6 = LLp.m6 - se;
	se = 0;
	LD.m6 = (LLp.m6 - se) / UD.m0;
	se = 0;
	se += LD.m12 * UD.m0;
	se += LD.m13 * UD.m6;
	UD.m12 = LLp.m12 - se;
	se = 0;
	LD.m12 = (LLp.m12 - se) / UD.m0;
	se = 0;
	se += LD.m18 * UD.m0;
	se += LD.m19 * UD.m6;
	se += LD.m20 * UD.m12;
	UD.m18 = LLp.m18 - se;
	se = 0;
	LD.m18 = (LLp.m18 - se) / UD.m0;
	se = 0;
	se += LD.m24 * UD.m0;
	se += LD.m25 * UD.m6;
	se += LD.m26 * UD.m12;
	se += LD.m27 * UD.m18;
	UD.m24 = LLp.m24 - se;
	se = 0;
	LD.m24 = (LLp.m24 - se) / UD.m0;
	se = 0;
	se += LD.m30 * UD.m0;
	se += LD.m31 * UD.m6;
	se += LD.m32 * UD.m12;
	se += LD.m33 * UD.m18;
	se += LD.m34 * UD.m24;
	UD.m30 = LLp.m30 - se;
	se = 0;
	LD.m30 = (LLp.m30 - se) / UD.m0;
	se = 0;
	se += LD.m6 * UD.m1;
	UD.m7 = LLp.m7 - se;
	se = 0;
	se += LD.m6 * UD.m1;
	LD.m7 = (LLp.m7 - se) / UD.m7;
	se = 0;
	se += LD.m12 * UD.m1;
	se += LD.m13 * UD.m7;
	UD.m13 = LLp.m13 - se;
	se = 0;
	se += LD.m12 * UD.m1;
	LD.m13 = (LLp.m13 - se) / UD.m7;
	se = 0;
	se += LD.m18 * UD.m1;
	se += LD.m19 * UD.m7;
	se += LD.m20 * UD.m13;
	UD.m19 = LLp.m19 - se;
	se = 0;
	se += LD.m18 * UD.m1;
	LD.m19 = (LLp.m19 - se) / UD.m7;
	se = 0;
	se += LD.m24 * UD.m1;
	se += LD.m25 * UD.m7;
	se += LD.m26 * UD.m13;
	se += LD.m27 * UD.m19;
	UD.m25 = LLp.m25 - se;
	se = 0;
	se += LD.m24 * UD.m1;
	LD.m25 = (LLp.m25 - se) / UD.m7;
	se = 0;
	se += LD.m30 * UD.m1;
	se += LD.m31 * UD.m7;
	se += LD.m32 * UD.m13;
	se += LD.m33 * UD.m19;
	se += LD.m34 * UD.m25;
	UD.m31 = LLp.m31 - se;
	se = 0;
	se += LD.m30 * UD.m1;
	LD.m31 = (LLp.m31 - se) / UD.m7;
	se = 0;
	se += LD.m12 * UD.m2;
	se += LD.m13 * UD.m8;
	UD.m14 = LLp.m14 - se;
	se = 0;
	se += LD.m12 * UD.m2;
	se += LD.m13 * UD.m8;
	LD.m14 = (LLp.m14 - se) / UD.m14;
	se = 0;
	se += LD.m18 * UD.m2;
	se += LD.m19 * UD.m8;
	se += LD.m20 * UD.m14;
	UD.m20 = LLp.m20 - se;
	se = 0;
	se += LD.m18 * UD.m2;
	se += LD.m19 * UD.m8;
	LD.m20 = (LLp.m20 - se) / UD.m14;
	se = 0;
	se += LD.m24 * UD.m2;
	se += LD.m25 * UD.m8;
	se += LD.m26 * UD.m14;
	se += LD.m27 * UD.m20;
	UD.m26 = LLp.m26 - se;
	se = 0;
	se += LD.m24 * UD.m2;
	se += LD.m25 * UD.m8;
	LD.m26 = (LLp.m26 - se) / UD.m14;
	se = 0;
	se += LD.m30 * UD.m2;
	se += LD.m31 * UD.m8;
	se += LD.m32 * UD.m14;
	se += LD.m33 * UD.m20;
	se += LD.m34 * UD.m26;
	UD.m32 = LLp.m32 - se;
	se = 0;
	se += LD.m30 * UD.m2;
	se += LD.m31 * UD.m8;
	LD.m32 = (LLp.m32 - se) / UD.m14;
	se = 0;
	se += LD.m18 * UD.m3;
	se += LD.m19 * UD.m9;
	se += LD.m20 * UD.m15;
	UD.m21 = LLp.m21 - se;
	se = 0;
	se += LD.m18 * UD.m3;
	se += LD.m19 * UD.m9;
	se += LD.m20 * UD.m15;
	LD.m21 = (LLp.m21 - se) / UD.m21;
	se = 0;
	se += LD.m24 * UD.m3;
	se += LD.m25 * UD.m9;
	se += LD.m26 * UD.m15;
	se += LD.m27 * UD.m21;
	UD.m27 = LLp.m27 - se;
	se = 0;
	se += LD.m24 * UD.m3;
	se += LD.m25 * UD.m9;
	se += LD.m26 * UD.m15;
	LD.m27 = (LLp.m27 - se) / UD.m21;
	se = 0;
	se += LD.m30 * UD.m3;
	se += LD.m31 * UD.m9;
	se += LD.m32 * UD.m15;
	se += LD.m33 * UD.m21;
	se += LD.m34 * UD.m27;
	UD.m33 = LLp.m33 - se;
	se = 0;
	se += LD.m30 * UD.m3;
	se += LD.m31 * UD.m9;
	se += LD.m32 * UD.m15;
	LD.m33 = (LLp.m33 - se) / UD.m21;
	se = 0;
	se += LD.m24 * UD.m4;
	se += LD.m25 * UD.m10;
	se += LD.m26 * UD.m16;
	se += LD.m27 * UD.m22;
	UD.m28 = LLp.m28 - se;
	se = 0;
	se += LD.m24 * UD.m4;
	se += LD.m25 * UD.m10;
	se += LD.m26 * UD.m16;
	se += LD.m27 * UD.m22;
	LD.m28 = (LLp.m28 - se) / UD.m28;
	se = 0;
	se += LD.m30 * UD.m4;
	se += LD.m31 * UD.m10;
	se += LD.m32 * UD.m16;
	se += LD.m33 * UD.m22;
	se += LD.m34 * UD.m28;
	UD.m34 = LLp.m34 - se;
	se = 0;
	se += LD.m30 * UD.m4;
	se += LD.m31 * UD.m10;
	se += LD.m32 * UD.m16;
	se += LD.m33 * UD.m22;
	LD.m34 = (LLp.m34 - se) / UD.m28;
	se = 0;
	se += LD.m30 * UD.m5;
	se += LD.m31 * UD.m11;
	se += LD.m32 * UD.m17;
	se += LD.m33 * UD.m23;
	se += LD.m34 * UD.m29;
	UD.m35 = LLp.m35 - se;
	se = 0;
	se += LD.m30 * UD.m5;
	se += LD.m31 * UD.m11;
	se += LD.m32 * UD.m17;
	se += LD.m33 * UD.m23;
	se += LD.m34 * UD.m29;
	LD.m35 = (LLp.m35 - se) / UD.m35;


      Big_Matrix I = {(N*N + M),(N*N + M), 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


      	I.m0 = (fixed_point_precision_16_16)1.0;
	I.m7 = (fixed_point_precision_16_16)1.0;
	I.m14 = (fixed_point_precision_16_16)1.0;
	I.m21 = (fixed_point_precision_16_16)1.0;
	I.m28 = (fixed_point_precision_16_16)1.0;
	I.m35 = (fixed_point_precision_16_16)1.0;


      //Big_Matrix_List LU_List = {2, , PD};


      // end decomp

      //Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
      Big_Matrix LU = {(N*N + M),(N*N + M), 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

      	LU.m0 = LD.m0 + UD.m0 - I.m0;
	LU.m1 = LD.m1 + UD.m1 - I.m1;
	LU.m2 = LD.m2 + UD.m2 - I.m2;
	LU.m3 = LD.m3 + UD.m3 - I.m3;
	LU.m4 = LD.m4 + UD.m4 - I.m4;
	LU.m5 = LD.m5 + UD.m5 - I.m5;
	LU.m6 = LD.m6 + UD.m6 - I.m6;
	LU.m7 = LD.m7 + UD.m7 - I.m7;
	LU.m8 = LD.m8 + UD.m8 - I.m8;
	LU.m9 = LD.m9 + UD.m9 - I.m9;
	LU.m10 = LD.m10 + UD.m10 - I.m10;
	LU.m11 = LD.m11 + UD.m11 - I.m11;
	LU.m12 = LD.m12 + UD.m12 - I.m12;
	LU.m13 = LD.m13 + UD.m13 - I.m13;
	LU.m14 = LD.m14 + UD.m14 - I.m14;
	LU.m15 = LD.m15 + UD.m15 - I.m15;
	LU.m16 = LD.m16 + UD.m16 - I.m16;
	LU.m17 = LD.m17 + UD.m17 - I.m17;
	LU.m18 = LD.m18 + UD.m18 - I.m18;
	LU.m19 = LD.m19 + UD.m19 - I.m19;
	LU.m20 = LD.m20 + UD.m20 - I.m20;
	LU.m21 = LD.m21 + UD.m21 - I.m21;
	LU.m22 = LD.m22 + UD.m22 - I.m22;
	LU.m23 = LD.m23 + UD.m23 - I.m23;
	LU.m24 = LD.m24 + UD.m24 - I.m24;
	LU.m25 = LD.m25 + UD.m25 - I.m25;
	LU.m26 = LD.m26 + UD.m26 - I.m26;
	LU.m27 = LD.m27 + UD.m27 - I.m27;
	LU.m28 = LD.m28 + UD.m28 - I.m28;
	LU.m29 = LD.m29 + UD.m29 - I.m29;
	LU.m30 = LD.m30 + UD.m30 - I.m30;
	LU.m31 = LD.m31 + UD.m31 - I.m31;
	LU.m32 = LD.m32 + UD.m32 - I.m32;
	LU.m33 = LD.m33 + UD.m33 - I.m33;
	LU.m34 = LD.m34 + UD.m34 - I.m34;
	LU.m35 = LD.m35 + UD.m35 - I.m35;



        //Big_Vector z = vec_mul_big(PD,e);
      Big_Vector z = {PD.cols, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


      	z.m0 += PD.m0 * e.m0;
	z.m0 += PD.m1 * e.m1;
	z.m0 += PD.m2 * e.m2;
	z.m0 += PD.m3 * e.m3;
	z.m0 += PD.m4 * e.m4;
	z.m0 += PD.m5 * e.m5;
	z.m1 += PD.m6 * e.m0;
	z.m1 += PD.m7 * e.m1;
	z.m1 += PD.m8 * e.m2;
	z.m1 += PD.m9 * e.m3;
	z.m1 += PD.m10 * e.m4;
	z.m1 += PD.m11 * e.m5;
	z.m2 += PD.m12 * e.m0;
	z.m2 += PD.m13 * e.m1;
	z.m2 += PD.m14 * e.m2;
	z.m2 += PD.m15 * e.m3;
	z.m2 += PD.m16 * e.m4;
	z.m2 += PD.m17 * e.m5;
	z.m3 += PD.m18 * e.m0;
	z.m3 += PD.m19 * e.m1;
	z.m3 += PD.m20 * e.m2;
	z.m3 += PD.m21 * e.m3;
	z.m3 += PD.m22 * e.m4;
	z.m3 += PD.m23 * e.m5;
	z.m4 += PD.m24 * e.m0;
	z.m4 += PD.m25 * e.m1;
	z.m4 += PD.m26 * e.m2;
	z.m4 += PD.m27 * e.m3;
	z.m4 += PD.m28 * e.m4;
	z.m4 += PD.m29 * e.m5;
	z.m5 += PD.m30 * e.m0;
	z.m5 += PD.m31 * e.m1;
	z.m5 += PD.m32 * e.m2;
	z.m5 += PD.m33 * e.m3;
	z.m5 += PD.m34 * e.m4;
	z.m5 += PD.m35 * e.m5;


      // forward substitute
      	z.m1 += z.m1 - (LU.m6 * z.m0);
	z.m2 += z.m2 - (LU.m12 * z.m0);
	z.m3 += z.m3 - (LU.m18 * z.m0);
	z.m4 += z.m4 - (LU.m24 * z.m0);
	z.m5 += z.m5 - (LU.m30 * z.m0);
	z.m2 += z.m2 - (LU.m13 * z.m1);
	z.m3 += z.m3 - (LU.m19 * z.m1);
	z.m4 += z.m4 - (LU.m25 * z.m1);
	z.m5 += z.m5 - (LU.m31 * z.m1);
	z.m3 += z.m3 - (LU.m20 * z.m2);
	z.m4 += z.m4 - (LU.m26 * z.m2);
	z.m5 += z.m5 - (LU.m32 * z.m2);
	z.m4 += z.m4 - (LU.m27 * z.m3);
	z.m5 += z.m5 - (LU.m33 * z.m3);
	z.m5 += z.m5 - (LU.m34 * z.m4);


      // backwards substitute
      	z.m5 = z.m5 / LU.m35;
	z.m0 += z.m0 - (LU.m5 * z.m5);
	z.m1 += z.m1 - (LU.m11 * z.m5);
	z.m2 += z.m2 - (LU.m17 * z.m5);
	z.m3 += z.m3 - (LU.m23 * z.m5);
	z.m4 += z.m4 - (LU.m29 * z.m5);
	z.m4 = z.m4 / LU.m28;
	z.m0 += z.m0 - (LU.m4 * z.m4);
	z.m1 += z.m1 - (LU.m10 * z.m4);
	z.m2 += z.m2 - (LU.m16 * z.m4);
	z.m3 += z.m3 - (LU.m22 * z.m4);
	z.m3 = z.m3 / LU.m21;
	z.m0 += z.m0 - (LU.m3 * z.m3);
	z.m1 += z.m1 - (LU.m9 * z.m3);
	z.m2 += z.m2 - (LU.m15 * z.m3);
	z.m2 = z.m2 / LU.m14;
	z.m0 += z.m0 - (LU.m2 * z.m2);
	z.m1 += z.m1 - (LU.m8 * z.m2);
	z.m1 = z.m1 / LU.m7;
	z.m0 += z.m0 - (LU.m1 * z.m1);


      /*for (int i = (n-1); i >= 0; i--){
        if (abs_val(get_big(i,i,LU)) < epsilon){
          valid = 0;
        }
        z.v[i] = z.v[i] / get_big(i,i,LU);
        for (int j = 0; j < i; j++){
          z.v[j] = z.v[j] - (get_big(j,i,LU) * z.v[i]);
        }
      }*/

      //end LUP solve

      printf("end LUP solve\n");

      //printf("Z");
      ////print_vec_b(z);

      //norm - for res error;
      //Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
      //fixed_point_precision_16_16 err = norm_vec_big(res);
      fixed_point_precision_16_16 err = norm_circ(e, LL, z);


      printf("end norm circ\n");
      	D.m0 = z.m0;
	D.m1 = z.m1;
	D.m2 = z.m2;
	D.m3 = z.m3;


      D = scal_mul(mat_add(D,transpose(D)),(fixed_point_precision_16_16)0.5);
      /*Matrix T = D;
      for (int it = 0; it < T.rows; it++) {
        for (int jt = 0; jt < T.cols; jt++) {
            T.m[it*(T.cols)+jt] = get(jt,it,D);
        }
      }
      for (int kt = 0; kt < D.rows*D.cols; kt++){
        D.m[kt] = (D.m[kt] + T.m[kt]) * 0.5;
      }*/


      	new_y.m0 = z.m4;
	new_y.m1 = z.m5;




      // end solve_eq
      printf("Y");
      //print_vec(new_y);

      printf("D");
      print_mat(D);

      //if (err <= 1e-2){

        Q.y = new_y;

        //Matrix S = mat_sub(C, mat_comb(y,A));

        //Matrix I = mat_mul(mat_mul(inverse(L),D),inverse(transpose(L)));
        //stop = norm_mat(I);
        stop = norm_mat_circ(L, D);

        printf("STOP %f\n", stop);

        if (stop > 0.25){
          fixed_point_precision_16_16 alpha = 0.2 / stop;

          Matrix XaD = mat_add(Q.X,scal_mul(D,alpha));
          /*for (int xx = 0; xx < D.rows*D.cols; xx++){
            XaD.m[xx] = Q.X.m[xx] + (D.m[xx] * alpha);
          }*/
          while (!psd(XaD)){
            alpha = alpha * (fixed_point_precision_16_16)0.5;


            printf("QX TRANS");
            print_mat(Q.X);

            XaD = mat_add(Q.X,scal_mul(D,alpha));
            /*for (int xx = 0; xx < D.rows*D.cols; xx++){
              XaD.m[xx] = Q.X.m[xx] + (D.m[xx] * alpha);
            }*/

          }

          Q.X = mat_add(Q.X,scal_mul(D,alpha));
          /*for (int xx = 0; xx < D.rows*D.cols; xx++){
            Q.X.m[xx] = Q.X.m[xx] + (D.m[xx] * alpha);
          }

          printf("QX TRANS");
          print_mat(Q.X);
          */

        //}
      }



    } // end while


		//solving - real SDP
    fixed_point_precision_16_16 errs = 0.0;
    while(0 && !(errs > 1e-2) && !(theta < 1e-4)){

      printf("reg X");
      print_mat(Q.X);

			//2. shrink T (Theta);
      fixed_point_precision_16_16 alpha = 0.8; //1 - ((sqrt(beta) - beta)/(sqrt(b)+sqrt(n))); //alpha in (0, 1)

      theta = alpha*theta;

      //3. compute newton direction and multipliers
      // factor Xb = L * Lt
      // solve system of equations

      //Eq_Sol sols = solve_eq(Q.X,A,C,theta);

      Matrix D0 = {0,0,0,0};


      //set up
      //Matrix U = mat_sub(Q.X,scal_div(mat_mul(mat_mul(Q.X,C),Q.X),theta));

      Matrix U10 = {0,0,0,0};


      	U10.m0 += Q.X.m0 * C.m0;
	U10.m0 += Q.X.m1 * C.m2;
	U10.m1 += Q.X.m0 * C.m1;
	U10.m1 += Q.X.m1 * C.m3;
	U10.m2 += Q.X.m2 * C.m0;
	U10.m2 += Q.X.m3 * C.m2;
	U10.m3 += Q.X.m2 * C.m1;
	U10.m3 += Q.X.m3 * C.m3;


      Matrix U0 = {0,0,0,0};

      	U0.m0 += U10.m0 * Q.X.m0;
	U0.m0 += U10.m1 * Q.X.m2;
	U0.m1 += U10.m0 * Q.X.m1;
	U0.m1 += U10.m1 * Q.X.m3;
	U0.m2 += U10.m2 * Q.X.m0;
	U0.m2 += U10.m3 * Q.X.m2;
	U0.m3 += U10.m2 * Q.X.m1;
	U0.m3 += U10.m3 * Q.X.m3;


      	U0.m0 = Q.X.m0 - ( U0.m0 / theta);
	U0.m1 = Q.X.m1 - ( U0.m1 / theta);
	U0.m2 = Q.X.m2 - ( U0.m2 / theta);
	U0.m3 = Q.X.m3 - ( U0.m3 / theta);


      Big_Vector K0 = {N*N+M,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


      	K0.m0 = U0.m0;
	K0.m1 = U0.m1;
	K0.m2 = U0.m2;
	K0.m3 = U0.m3;



      Big_Matrix QQ0 = {N*N,M,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

      Matrix nX0 = {0,0,0,0};


      	nX0.m0 = Q.X.m0* -1;
	nX0.m1 = Q.X.m1* -1;
	nX0.m2 = Q.X.m2* -1;
	nX0.m3 = Q.X.m3* -1;



      //for(int a = 0; a < A.len; a++){
      //AQ.m0 = scal_div(mat_mul(mat_mul(nX,AQ.m0),Q.X),theta);
      Matrix ATz0 = {0,0,0,0};
Matrix ATz1 = {0,0,0,0};
	ATz0.m0 += nX0.m0 * A.m0.m0;
	ATz1.m0 += nX0.m0 * A.m1.m0;
	ATz0.m0 += nX0.m1 * A.m0.m2;
	ATz1.m0 += nX0.m1 * A.m1.m2;
	ATz0.m1 += nX0.m0 * A.m0.m1;
	ATz1.m1 += nX0.m0 * A.m1.m1;
	ATz0.m1 += nX0.m1 * A.m0.m3;
	ATz1.m1 += nX0.m1 * A.m1.m3;
	ATz0.m2 += nX0.m2 * A.m0.m0;
	ATz1.m2 += nX0.m2 * A.m1.m0;
	ATz0.m2 += nX0.m3 * A.m0.m2;
	ATz1.m2 += nX0.m3 * A.m1.m2;
	ATz0.m3 += nX0.m2 * A.m0.m1;
	ATz1.m3 += nX0.m2 * A.m1.m1;
	ATz0.m3 += nX0.m3 * A.m0.m3;
	ATz1.m3 += nX0.m3 * A.m1.m3;


      Matrix ATzz0 = {0,0,0,0};
Matrix ATzz1 = {0,0,0,0};
	ATzz0.m0 += ATz0.m0 * Q.X.m0;
	ATzz1.m0 += ATz1.m0 * Q.X.m0;
	ATzz0.m0 += ATz0.m1 * Q.X.m2;
	ATzz1.m0 += ATz1.m1 * Q.X.m2;
	ATzz0.m1 += ATz0.m0 * Q.X.m1;
	ATzz1.m1 += ATz1.m0 * Q.X.m1;
	ATzz0.m1 += ATz0.m1 * Q.X.m3;
	ATzz1.m1 += ATz1.m1 * Q.X.m3;
	ATzz0.m2 += ATz0.m2 * Q.X.m0;
	ATzz1.m2 += ATz1.m2 * Q.X.m0;
	ATzz0.m2 += ATz0.m3 * Q.X.m2;
	ATzz1.m2 += ATz1.m3 * Q.X.m2;
	ATzz0.m3 += ATz0.m2 * Q.X.m1;
	ATzz1.m3 += ATz1.m2 * Q.X.m1;
	ATzz0.m3 += ATz0.m3 * Q.X.m3;
	ATzz1.m3 += ATz1.m3 * Q.X.m3;


      	ATzz0.m0 = ATzz0.m0 / theta;
	ATzz1.m0 = ATzz1.m0 / theta;
	ATzz0.m1 = ATzz0.m1 / theta;
	ATzz1.m1 = ATzz1.m1 / theta;
	ATzz0.m2 = ATzz0.m2 / theta;
	ATzz1.m2 = ATzz1.m2 / theta;
	ATzz0.m3 = ATzz0.m3 / theta;
	ATzz1.m3 = ATzz1.m3 / theta;


      //AQ.m0 = flatten(AQ.m0);


      //AQ.m1 = scal_div(mat_mul(mat_mul(nX,AQ.m1),Q.X),theta);
      //AQ.m1 = flatten(AQ.m1);

      //}
      	QQ0.m0 = ATzz0.m0;
	QQ0.m1 = ATzz1.m0;
	QQ0.m2 = ATzz0.m1;
	QQ0.m3 = ATzz1.m1;
	QQ0.m4 = ATzz0.m2;
	QQ0.m5 = ATzz1.m2;
	QQ0.m6 = ATzz0.m3;
	QQ0.m7 = ATzz1.m3;


      Big_Matrix R0 = {M,N*N,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

      	R0.m0 = A.m0.m0;
	R0.m4 = A.m1.m0;
	R0.m1 = A.m0.m1;
	R0.m5 = A.m1.m1;
	R0.m2 = A.m0.m2;
	R0.m6 = A.m1.m2;
	R0.m3 = A.m0.m3;
	R0.m7 = A.m1.m3;


      Big_Matrix LL0 = {(N*N + M),(N*N + M),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

        //prepend identity matrix to Q
      	LL0.m0 = (fixed_point_precision_16_16)1.0;
	LL0.m4 = QQ0.m0;
	LL0.m5 = QQ0.m1;
	LL0.m7 = (fixed_point_precision_16_16)1.0;
	LL0.m10 = QQ0.m2;
	LL0.m11 = QQ0.m3;
	LL0.m14 = (fixed_point_precision_16_16)1.0;
	LL0.m16 = QQ0.m4;
	LL0.m17 = QQ0.m5;
	LL0.m21 = (fixed_point_precision_16_16)1.0;
	LL0.m22 = QQ0.m6;
	LL0.m23 = QQ0.m7;

    // append P, zeros
      	LL0.m28 = (fixed_point_precision_16_16)0.0;
	LL0.m29 = (fixed_point_precision_16_16)0.0;
	LL0.m34 = (fixed_point_precision_16_16)0.0;
	LL0.m35 = (fixed_point_precision_16_16)0.0;


          //least sq solution
          //Big_Vector e = vectorize(K);
          Big_Vector e0 = {10*N*N+M, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


          	e0.m0 = K0.m0;
	e0.m1 = K0.m1;
	e0.m2 = K0.m2;
	e0.m3 = K0.m3;
	e0.m4 = K0.m4;
	e0.m5 = K0.m5;


          //Big_Vector z = LUP_solve(LL, e);
          //Big_Matrix_List LU_List = LUP_decompose_big(LL);
		      Big_Matrix LD0 = {LL0.rows,LL0.cols,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

		      Big_Matrix UD0 = {LL0.rows,LL0.cols,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

		      Big_Matrix PD0 = {LL0.rows,LL0.cols,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

		      int n0 = LL0.rows;

		      //pivot A P
		      	PD0.m0 = (fixed_point_precision_16_16)1.0;
	PD0.m7 = (fixed_point_precision_16_16)1.0;
	PD0.m14 = (fixed_point_precision_16_16)1.0;
	PD0.m21 = (fixed_point_precision_16_16)1.0;
	PD0.m28 = (fixed_point_precision_16_16)1.0;
	PD0.m35 = (fixed_point_precision_16_16)1.0;





						fixed_point_precision_16_16 cmp0; int max_j0;
	max_j0 = 0;
	if (abs_val(LL0.m0) > abs_val(cmp0)){ max_j0 = 0; }
	if (abs_val(LL0.m6) > abs_val(cmp0)){ max_j0 = 1; }
	if (abs_val(LL0.m12) > abs_val(cmp0)){ max_j0 = 2; }
	if (abs_val(LL0.m18) > abs_val(cmp0)){ max_j0 = 3; }
	if (abs_val(LL0.m24) > abs_val(cmp0)){ max_j0 = 4; }
	if (abs_val(LL0.m30) > abs_val(cmp0)){ max_j0 = 5; }
if (max_j0 == 0){}
if (max_j0 == 1){	fixed_point_precision_16_16 temp00;
	temp00 = PD0.m0;
	PD0.m0 = PD0.m6;
	PD0.m6 = temp00;
	temp00 = PD0.m1;
	PD0.m1 = PD0.m7;
	PD0.m7 = temp00;
	temp00 = PD0.m2;
	PD0.m2 = PD0.m8;
	PD0.m8 = temp00;
	temp00 = PD0.m3;
	PD0.m3 = PD0.m9;
	PD0.m9 = temp00;
	temp00 = PD0.m4;
	PD0.m4 = PD0.m10;
	PD0.m10 = temp00;
	temp00 = PD0.m5;
	PD0.m5 = PD0.m11;
	PD0.m11 = temp00;
}
if (max_j0 == 2){	fixed_point_precision_16_16 temp00;
	temp00 = PD0.m0;
	PD0.m0 = PD0.m12;
	PD0.m12 = temp00;
	temp00 = PD0.m1;
	PD0.m1 = PD0.m13;
	PD0.m13 = temp00;
	temp00 = PD0.m2;
	PD0.m2 = PD0.m14;
	PD0.m14 = temp00;
	temp00 = PD0.m3;
	PD0.m3 = PD0.m15;
	PD0.m15 = temp00;
	temp00 = PD0.m4;
	PD0.m4 = PD0.m16;
	PD0.m16 = temp00;
	temp00 = PD0.m5;
	PD0.m5 = PD0.m17;
	PD0.m17 = temp00;
}
if (max_j0 == 3){	fixed_point_precision_16_16 temp00;
	temp00 = PD0.m0;
	PD0.m0 = PD0.m18;
	PD0.m18 = temp00;
	temp00 = PD0.m1;
	PD0.m1 = PD0.m19;
	PD0.m19 = temp00;
	temp00 = PD0.m2;
	PD0.m2 = PD0.m20;
	PD0.m20 = temp00;
	temp00 = PD0.m3;
	PD0.m3 = PD0.m21;
	PD0.m21 = temp00;
	temp00 = PD0.m4;
	PD0.m4 = PD0.m22;
	PD0.m22 = temp00;
	temp00 = PD0.m5;
	PD0.m5 = PD0.m23;
	PD0.m23 = temp00;
}
if (max_j0 == 4){	fixed_point_precision_16_16 temp00;
	temp00 = PD0.m0;
	PD0.m0 = PD0.m24;
	PD0.m24 = temp00;
	temp00 = PD0.m1;
	PD0.m1 = PD0.m25;
	PD0.m25 = temp00;
	temp00 = PD0.m2;
	PD0.m2 = PD0.m26;
	PD0.m26 = temp00;
	temp00 = PD0.m3;
	PD0.m3 = PD0.m27;
	PD0.m27 = temp00;
	temp00 = PD0.m4;
	PD0.m4 = PD0.m28;
	PD0.m28 = temp00;
	temp00 = PD0.m5;
	PD0.m5 = PD0.m29;
	PD0.m29 = temp00;
}
if (max_j0 == 5){	fixed_point_precision_16_16 temp00;
	temp00 = PD0.m0;
	PD0.m0 = PD0.m30;
	PD0.m30 = temp00;
	temp00 = PD0.m1;
	PD0.m1 = PD0.m31;
	PD0.m31 = temp00;
	temp00 = PD0.m2;
	PD0.m2 = PD0.m32;
	PD0.m32 = temp00;
	temp00 = PD0.m3;
	PD0.m3 = PD0.m33;
	PD0.m33 = temp00;
	temp00 = PD0.m4;
	PD0.m4 = PD0.m34;
	PD0.m34 = temp00;
	temp00 = PD0.m5;
	PD0.m5 = PD0.m35;
	PD0.m35 = temp00;
}
	max_j0 = 1;
	if (abs_val(LL0.m1) > abs_val(cmp0)){ max_j0 = 0; }
	if (abs_val(LL0.m7) > abs_val(cmp0)){ max_j0 = 1; }
	if (abs_val(LL0.m13) > abs_val(cmp0)){ max_j0 = 2; }
	if (abs_val(LL0.m19) > abs_val(cmp0)){ max_j0 = 3; }
	if (abs_val(LL0.m25) > abs_val(cmp0)){ max_j0 = 4; }
	if (abs_val(LL0.m31) > abs_val(cmp0)){ max_j0 = 5; }
if (max_j0 == 0){	fixed_point_precision_16_16 temp01;
	temp01 = PD0.m6;
	PD0.m6 = PD0.m0;
	PD0.m0 = temp01;
	temp01 = PD0.m7;
	PD0.m7 = PD0.m1;
	PD0.m1 = temp01;
	temp01 = PD0.m8;
	PD0.m8 = PD0.m2;
	PD0.m2 = temp01;
	temp01 = PD0.m9;
	PD0.m9 = PD0.m3;
	PD0.m3 = temp01;
	temp01 = PD0.m10;
	PD0.m10 = PD0.m4;
	PD0.m4 = temp01;
	temp01 = PD0.m11;
	PD0.m11 = PD0.m5;
	PD0.m5 = temp01;
}
if (max_j0 == 1){}
if (max_j0 == 2){	fixed_point_precision_16_16 temp01;
	temp01 = PD0.m6;
	PD0.m6 = PD0.m12;
	PD0.m12 = temp01;
	temp01 = PD0.m7;
	PD0.m7 = PD0.m13;
	PD0.m13 = temp01;
	temp01 = PD0.m8;
	PD0.m8 = PD0.m14;
	PD0.m14 = temp01;
	temp01 = PD0.m9;
	PD0.m9 = PD0.m15;
	PD0.m15 = temp01;
	temp01 = PD0.m10;
	PD0.m10 = PD0.m16;
	PD0.m16 = temp01;
	temp01 = PD0.m11;
	PD0.m11 = PD0.m17;
	PD0.m17 = temp01;
}
if (max_j0 == 3){	fixed_point_precision_16_16 temp01;
	temp01 = PD0.m6;
	PD0.m6 = PD0.m18;
	PD0.m18 = temp01;
	temp01 = PD0.m7;
	PD0.m7 = PD0.m19;
	PD0.m19 = temp01;
	temp01 = PD0.m8;
	PD0.m8 = PD0.m20;
	PD0.m20 = temp01;
	temp01 = PD0.m9;
	PD0.m9 = PD0.m21;
	PD0.m21 = temp01;
	temp01 = PD0.m10;
	PD0.m10 = PD0.m22;
	PD0.m22 = temp01;
	temp01 = PD0.m11;
	PD0.m11 = PD0.m23;
	PD0.m23 = temp01;
}
if (max_j0 == 4){	fixed_point_precision_16_16 temp01;
	temp01 = PD0.m6;
	PD0.m6 = PD0.m24;
	PD0.m24 = temp01;
	temp01 = PD0.m7;
	PD0.m7 = PD0.m25;
	PD0.m25 = temp01;
	temp01 = PD0.m8;
	PD0.m8 = PD0.m26;
	PD0.m26 = temp01;
	temp01 = PD0.m9;
	PD0.m9 = PD0.m27;
	PD0.m27 = temp01;
	temp01 = PD0.m10;
	PD0.m10 = PD0.m28;
	PD0.m28 = temp01;
	temp01 = PD0.m11;
	PD0.m11 = PD0.m29;
	PD0.m29 = temp01;
}
if (max_j0 == 5){	fixed_point_precision_16_16 temp01;
	temp01 = PD0.m6;
	PD0.m6 = PD0.m30;
	PD0.m30 = temp01;
	temp01 = PD0.m7;
	PD0.m7 = PD0.m31;
	PD0.m31 = temp01;
	temp01 = PD0.m8;
	PD0.m8 = PD0.m32;
	PD0.m32 = temp01;
	temp01 = PD0.m9;
	PD0.m9 = PD0.m33;
	PD0.m33 = temp01;
	temp01 = PD0.m10;
	PD0.m10 = PD0.m34;
	PD0.m34 = temp01;
	temp01 = PD0.m11;
	PD0.m11 = PD0.m35;
	PD0.m35 = temp01;
}
	max_j0 = 2;
	if (abs_val(LL0.m2) > abs_val(cmp0)){ max_j0 = 0; }
	if (abs_val(LL0.m8) > abs_val(cmp0)){ max_j0 = 1; }
	if (abs_val(LL0.m14) > abs_val(cmp0)){ max_j0 = 2; }
	if (abs_val(LL0.m20) > abs_val(cmp0)){ max_j0 = 3; }
	if (abs_val(LL0.m26) > abs_val(cmp0)){ max_j0 = 4; }
	if (abs_val(LL0.m32) > abs_val(cmp0)){ max_j0 = 5; }
if (max_j0 == 0){	fixed_point_precision_16_16 temp02;
	temp02 = PD0.m12;
	PD0.m12 = PD0.m0;
	PD0.m0 = temp02;
	temp02 = PD0.m13;
	PD0.m13 = PD0.m1;
	PD0.m1 = temp02;
	temp02 = PD0.m14;
	PD0.m14 = PD0.m2;
	PD0.m2 = temp02;
	temp02 = PD0.m15;
	PD0.m15 = PD0.m3;
	PD0.m3 = temp02;
	temp02 = PD0.m16;
	PD0.m16 = PD0.m4;
	PD0.m4 = temp02;
	temp02 = PD0.m17;
	PD0.m17 = PD0.m5;
	PD0.m5 = temp02;
}
if (max_j0 == 1){	fixed_point_precision_16_16 temp02;
	temp02 = PD0.m12;
	PD0.m12 = PD0.m6;
	PD0.m6 = temp02;
	temp02 = PD0.m13;
	PD0.m13 = PD0.m7;
	PD0.m7 = temp02;
	temp02 = PD0.m14;
	PD0.m14 = PD0.m8;
	PD0.m8 = temp02;
	temp02 = PD0.m15;
	PD0.m15 = PD0.m9;
	PD0.m9 = temp02;
	temp02 = PD0.m16;
	PD0.m16 = PD0.m10;
	PD0.m10 = temp02;
	temp02 = PD0.m17;
	PD0.m17 = PD0.m11;
	PD0.m11 = temp02;
}
if (max_j0 == 2){}
if (max_j0 == 3){	fixed_point_precision_16_16 temp02;
	temp02 = PD0.m12;
	PD0.m12 = PD0.m18;
	PD0.m18 = temp02;
	temp02 = PD0.m13;
	PD0.m13 = PD0.m19;
	PD0.m19 = temp02;
	temp02 = PD0.m14;
	PD0.m14 = PD0.m20;
	PD0.m20 = temp02;
	temp02 = PD0.m15;
	PD0.m15 = PD0.m21;
	PD0.m21 = temp02;
	temp02 = PD0.m16;
	PD0.m16 = PD0.m22;
	PD0.m22 = temp02;
	temp02 = PD0.m17;
	PD0.m17 = PD0.m23;
	PD0.m23 = temp02;
}
if (max_j0 == 4){	fixed_point_precision_16_16 temp02;
	temp02 = PD0.m12;
	PD0.m12 = PD0.m24;
	PD0.m24 = temp02;
	temp02 = PD0.m13;
	PD0.m13 = PD0.m25;
	PD0.m25 = temp02;
	temp02 = PD0.m14;
	PD0.m14 = PD0.m26;
	PD0.m26 = temp02;
	temp02 = PD0.m15;
	PD0.m15 = PD0.m27;
	PD0.m27 = temp02;
	temp02 = PD0.m16;
	PD0.m16 = PD0.m28;
	PD0.m28 = temp02;
	temp02 = PD0.m17;
	PD0.m17 = PD0.m29;
	PD0.m29 = temp02;
}
if (max_j0 == 5){	fixed_point_precision_16_16 temp02;
	temp02 = PD0.m12;
	PD0.m12 = PD0.m30;
	PD0.m30 = temp02;
	temp02 = PD0.m13;
	PD0.m13 = PD0.m31;
	PD0.m31 = temp02;
	temp02 = PD0.m14;
	PD0.m14 = PD0.m32;
	PD0.m32 = temp02;
	temp02 = PD0.m15;
	PD0.m15 = PD0.m33;
	PD0.m33 = temp02;
	temp02 = PD0.m16;
	PD0.m16 = PD0.m34;
	PD0.m34 = temp02;
	temp02 = PD0.m17;
	PD0.m17 = PD0.m35;
	PD0.m35 = temp02;
}
	max_j0 = 3;
	if (abs_val(LL0.m3) > abs_val(cmp0)){ max_j0 = 0; }
	if (abs_val(LL0.m9) > abs_val(cmp0)){ max_j0 = 1; }
	if (abs_val(LL0.m15) > abs_val(cmp0)){ max_j0 = 2; }
	if (abs_val(LL0.m21) > abs_val(cmp0)){ max_j0 = 3; }
	if (abs_val(LL0.m27) > abs_val(cmp0)){ max_j0 = 4; }
	if (abs_val(LL0.m33) > abs_val(cmp0)){ max_j0 = 5; }
if (max_j0 == 0){	fixed_point_precision_16_16 temp03;
	temp03 = PD0.m18;
	PD0.m18 = PD0.m0;
	PD0.m0 = temp03;
	temp03 = PD0.m19;
	PD0.m19 = PD0.m1;
	PD0.m1 = temp03;
	temp03 = PD0.m20;
	PD0.m20 = PD0.m2;
	PD0.m2 = temp03;
	temp03 = PD0.m21;
	PD0.m21 = PD0.m3;
	PD0.m3 = temp03;
	temp03 = PD0.m22;
	PD0.m22 = PD0.m4;
	PD0.m4 = temp03;
	temp03 = PD0.m23;
	PD0.m23 = PD0.m5;
	PD0.m5 = temp03;
}
if (max_j0 == 1){	fixed_point_precision_16_16 temp03;
	temp03 = PD0.m18;
	PD0.m18 = PD0.m6;
	PD0.m6 = temp03;
	temp03 = PD0.m19;
	PD0.m19 = PD0.m7;
	PD0.m7 = temp03;
	temp03 = PD0.m20;
	PD0.m20 = PD0.m8;
	PD0.m8 = temp03;
	temp03 = PD0.m21;
	PD0.m21 = PD0.m9;
	PD0.m9 = temp03;
	temp03 = PD0.m22;
	PD0.m22 = PD0.m10;
	PD0.m10 = temp03;
	temp03 = PD0.m23;
	PD0.m23 = PD0.m11;
	PD0.m11 = temp03;
}
if (max_j0 == 2){	fixed_point_precision_16_16 temp03;
	temp03 = PD0.m18;
	PD0.m18 = PD0.m12;
	PD0.m12 = temp03;
	temp03 = PD0.m19;
	PD0.m19 = PD0.m13;
	PD0.m13 = temp03;
	temp03 = PD0.m20;
	PD0.m20 = PD0.m14;
	PD0.m14 = temp03;
	temp03 = PD0.m21;
	PD0.m21 = PD0.m15;
	PD0.m15 = temp03;
	temp03 = PD0.m22;
	PD0.m22 = PD0.m16;
	PD0.m16 = temp03;
	temp03 = PD0.m23;
	PD0.m23 = PD0.m17;
	PD0.m17 = temp03;
}
if (max_j0 == 3){}
if (max_j0 == 4){	fixed_point_precision_16_16 temp03;
	temp03 = PD0.m18;
	PD0.m18 = PD0.m24;
	PD0.m24 = temp03;
	temp03 = PD0.m19;
	PD0.m19 = PD0.m25;
	PD0.m25 = temp03;
	temp03 = PD0.m20;
	PD0.m20 = PD0.m26;
	PD0.m26 = temp03;
	temp03 = PD0.m21;
	PD0.m21 = PD0.m27;
	PD0.m27 = temp03;
	temp03 = PD0.m22;
	PD0.m22 = PD0.m28;
	PD0.m28 = temp03;
	temp03 = PD0.m23;
	PD0.m23 = PD0.m29;
	PD0.m29 = temp03;
}
if (max_j0 == 5){	fixed_point_precision_16_16 temp03;
	temp03 = PD0.m18;
	PD0.m18 = PD0.m30;
	PD0.m30 = temp03;
	temp03 = PD0.m19;
	PD0.m19 = PD0.m31;
	PD0.m31 = temp03;
	temp03 = PD0.m20;
	PD0.m20 = PD0.m32;
	PD0.m32 = temp03;
	temp03 = PD0.m21;
	PD0.m21 = PD0.m33;
	PD0.m33 = temp03;
	temp03 = PD0.m22;
	PD0.m22 = PD0.m34;
	PD0.m34 = temp03;
	temp03 = PD0.m23;
	PD0.m23 = PD0.m35;
	PD0.m35 = temp03;
}
	max_j0 = 4;
	if (abs_val(LL0.m4) > abs_val(cmp0)){ max_j0 = 0; }
	if (abs_val(LL0.m10) > abs_val(cmp0)){ max_j0 = 1; }
	if (abs_val(LL0.m16) > abs_val(cmp0)){ max_j0 = 2; }
	if (abs_val(LL0.m22) > abs_val(cmp0)){ max_j0 = 3; }
	if (abs_val(LL0.m28) > abs_val(cmp0)){ max_j0 = 4; }
	if (abs_val(LL0.m34) > abs_val(cmp0)){ max_j0 = 5; }
if (max_j0 == 0){	fixed_point_precision_16_16 temp04;
	temp04 = PD0.m24;
	PD0.m24 = PD0.m0;
	PD0.m0 = temp04;
	temp04 = PD0.m25;
	PD0.m25 = PD0.m1;
	PD0.m1 = temp04;
	temp04 = PD0.m26;
	PD0.m26 = PD0.m2;
	PD0.m2 = temp04;
	temp04 = PD0.m27;
	PD0.m27 = PD0.m3;
	PD0.m3 = temp04;
	temp04 = PD0.m28;
	PD0.m28 = PD0.m4;
	PD0.m4 = temp04;
	temp04 = PD0.m29;
	PD0.m29 = PD0.m5;
	PD0.m5 = temp04;
}
if (max_j0 == 1){	fixed_point_precision_16_16 temp04;
	temp04 = PD0.m24;
	PD0.m24 = PD0.m6;
	PD0.m6 = temp04;
	temp04 = PD0.m25;
	PD0.m25 = PD0.m7;
	PD0.m7 = temp04;
	temp04 = PD0.m26;
	PD0.m26 = PD0.m8;
	PD0.m8 = temp04;
	temp04 = PD0.m27;
	PD0.m27 = PD0.m9;
	PD0.m9 = temp04;
	temp04 = PD0.m28;
	PD0.m28 = PD0.m10;
	PD0.m10 = temp04;
	temp04 = PD0.m29;
	PD0.m29 = PD0.m11;
	PD0.m11 = temp04;
}
if (max_j0 == 2){	fixed_point_precision_16_16 temp04;
	temp04 = PD0.m24;
	PD0.m24 = PD0.m12;
	PD0.m12 = temp04;
	temp04 = PD0.m25;
	PD0.m25 = PD0.m13;
	PD0.m13 = temp04;
	temp04 = PD0.m26;
	PD0.m26 = PD0.m14;
	PD0.m14 = temp04;
	temp04 = PD0.m27;
	PD0.m27 = PD0.m15;
	PD0.m15 = temp04;
	temp04 = PD0.m28;
	PD0.m28 = PD0.m16;
	PD0.m16 = temp04;
	temp04 = PD0.m29;
	PD0.m29 = PD0.m17;
	PD0.m17 = temp04;
}
if (max_j0 == 3){	fixed_point_precision_16_16 temp04;
	temp04 = PD0.m24;
	PD0.m24 = PD0.m18;
	PD0.m18 = temp04;
	temp04 = PD0.m25;
	PD0.m25 = PD0.m19;
	PD0.m19 = temp04;
	temp04 = PD0.m26;
	PD0.m26 = PD0.m20;
	PD0.m20 = temp04;
	temp04 = PD0.m27;
	PD0.m27 = PD0.m21;
	PD0.m21 = temp04;
	temp04 = PD0.m28;
	PD0.m28 = PD0.m22;
	PD0.m22 = temp04;
	temp04 = PD0.m29;
	PD0.m29 = PD0.m23;
	PD0.m23 = temp04;
}
if (max_j0 == 4){}
if (max_j0 == 5){	fixed_point_precision_16_16 temp04;
	temp04 = PD0.m24;
	PD0.m24 = PD0.m30;
	PD0.m30 = temp04;
	temp04 = PD0.m25;
	PD0.m25 = PD0.m31;
	PD0.m31 = temp04;
	temp04 = PD0.m26;
	PD0.m26 = PD0.m32;
	PD0.m32 = temp04;
	temp04 = PD0.m27;
	PD0.m27 = PD0.m33;
	PD0.m33 = temp04;
	temp04 = PD0.m28;
	PD0.m28 = PD0.m34;
	PD0.m34 = temp04;
	temp04 = PD0.m29;
	PD0.m29 = PD0.m35;
	PD0.m35 = temp04;
}
	max_j0 = 5;
	if (abs_val(LL0.m5) > abs_val(cmp0)){ max_j0 = 0; }
	if (abs_val(LL0.m11) > abs_val(cmp0)){ max_j0 = 1; }
	if (abs_val(LL0.m17) > abs_val(cmp0)){ max_j0 = 2; }
	if (abs_val(LL0.m23) > abs_val(cmp0)){ max_j0 = 3; }
	if (abs_val(LL0.m29) > abs_val(cmp0)){ max_j0 = 4; }
	if (abs_val(LL0.m35) > abs_val(cmp0)){ max_j0 = 5; }
if (max_j0 == 0){	fixed_point_precision_16_16 temp05;
	temp05 = PD0.m30;
	PD0.m30 = PD0.m0;
	PD0.m0 = temp05;
	temp05 = PD0.m31;
	PD0.m31 = PD0.m1;
	PD0.m1 = temp05;
	temp05 = PD0.m32;
	PD0.m32 = PD0.m2;
	PD0.m2 = temp05;
	temp05 = PD0.m33;
	PD0.m33 = PD0.m3;
	PD0.m3 = temp05;
	temp05 = PD0.m34;
	PD0.m34 = PD0.m4;
	PD0.m4 = temp05;
	temp05 = PD0.m35;
	PD0.m35 = PD0.m5;
	PD0.m5 = temp05;
}
if (max_j0 == 1){	fixed_point_precision_16_16 temp05;
	temp05 = PD0.m30;
	PD0.m30 = PD0.m6;
	PD0.m6 = temp05;
	temp05 = PD0.m31;
	PD0.m31 = PD0.m7;
	PD0.m7 = temp05;
	temp05 = PD0.m32;
	PD0.m32 = PD0.m8;
	PD0.m8 = temp05;
	temp05 = PD0.m33;
	PD0.m33 = PD0.m9;
	PD0.m9 = temp05;
	temp05 = PD0.m34;
	PD0.m34 = PD0.m10;
	PD0.m10 = temp05;
	temp05 = PD0.m35;
	PD0.m35 = PD0.m11;
	PD0.m11 = temp05;
}
if (max_j0 == 2){	fixed_point_precision_16_16 temp05;
	temp05 = PD0.m30;
	PD0.m30 = PD0.m12;
	PD0.m12 = temp05;
	temp05 = PD0.m31;
	PD0.m31 = PD0.m13;
	PD0.m13 = temp05;
	temp05 = PD0.m32;
	PD0.m32 = PD0.m14;
	PD0.m14 = temp05;
	temp05 = PD0.m33;
	PD0.m33 = PD0.m15;
	PD0.m15 = temp05;
	temp05 = PD0.m34;
	PD0.m34 = PD0.m16;
	PD0.m16 = temp05;
	temp05 = PD0.m35;
	PD0.m35 = PD0.m17;
	PD0.m17 = temp05;
}
if (max_j0 == 3){	fixed_point_precision_16_16 temp05;
	temp05 = PD0.m30;
	PD0.m30 = PD0.m18;
	PD0.m18 = temp05;
	temp05 = PD0.m31;
	PD0.m31 = PD0.m19;
	PD0.m19 = temp05;
	temp05 = PD0.m32;
	PD0.m32 = PD0.m20;
	PD0.m20 = temp05;
	temp05 = PD0.m33;
	PD0.m33 = PD0.m21;
	PD0.m21 = temp05;
	temp05 = PD0.m34;
	PD0.m34 = PD0.m22;
	PD0.m22 = temp05;
	temp05 = PD0.m35;
	PD0.m35 = PD0.m23;
	PD0.m23 = temp05;
}
if (max_j0 == 4){	fixed_point_precision_16_16 temp05;
	temp05 = PD0.m30;
	PD0.m30 = PD0.m24;
	PD0.m24 = temp05;
	temp05 = PD0.m31;
	PD0.m31 = PD0.m25;
	PD0.m25 = temp05;
	temp05 = PD0.m32;
	PD0.m32 = PD0.m26;
	PD0.m26 = temp05;
	temp05 = PD0.m33;
	PD0.m33 = PD0.m27;
	PD0.m27 = temp05;
	temp05 = PD0.m34;
	PD0.m34 = PD0.m28;
	PD0.m28 = temp05;
	temp05 = PD0.m35;
	PD0.m35 = PD0.m29;
	PD0.m29 = temp05;
}
if (max_j0 == 5){}


					//Big_Matrix LLp = mat_mul_big(PD,LL);
		      Big_Matrix LLp0 = {PD0.rows,LL0.cols,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


		      	LLp0.m0 = PD0.m0 * LL0.m0;
	LLp0.m0 = PD0.m1 * LL0.m6;
	LLp0.m0 = PD0.m2 * LL0.m12;
	LLp0.m0 = PD0.m3 * LL0.m18;
	LLp0.m0 = PD0.m4 * LL0.m24;
	LLp0.m0 = PD0.m5 * LL0.m30;
	LLp0.m1 = PD0.m0 * LL0.m1;
	LLp0.m1 = PD0.m1 * LL0.m7;
	LLp0.m1 = PD0.m2 * LL0.m13;
	LLp0.m1 = PD0.m3 * LL0.m19;
	LLp0.m1 = PD0.m4 * LL0.m25;
	LLp0.m1 = PD0.m5 * LL0.m31;
	LLp0.m2 = PD0.m0 * LL0.m2;
	LLp0.m2 = PD0.m1 * LL0.m8;
	LLp0.m2 = PD0.m2 * LL0.m14;
	LLp0.m2 = PD0.m3 * LL0.m20;
	LLp0.m2 = PD0.m4 * LL0.m26;
	LLp0.m2 = PD0.m5 * LL0.m32;
	LLp0.m3 = PD0.m0 * LL0.m3;
	LLp0.m3 = PD0.m1 * LL0.m9;
	LLp0.m3 = PD0.m2 * LL0.m15;
	LLp0.m3 = PD0.m3 * LL0.m21;
	LLp0.m3 = PD0.m4 * LL0.m27;
	LLp0.m3 = PD0.m5 * LL0.m33;
	LLp0.m4 = PD0.m0 * LL0.m4;
	LLp0.m4 = PD0.m1 * LL0.m10;
	LLp0.m4 = PD0.m2 * LL0.m16;
	LLp0.m4 = PD0.m3 * LL0.m22;
	LLp0.m4 = PD0.m4 * LL0.m28;
	LLp0.m4 = PD0.m5 * LL0.m34;
	LLp0.m5 = PD0.m0 * LL0.m5;
	LLp0.m5 = PD0.m1 * LL0.m11;
	LLp0.m5 = PD0.m2 * LL0.m17;
	LLp0.m5 = PD0.m3 * LL0.m23;
	LLp0.m5 = PD0.m4 * LL0.m29;
	LLp0.m5 = PD0.m5 * LL0.m35;
	LLp0.m6 = PD0.m6 * LL0.m0;
	LLp0.m6 = PD0.m7 * LL0.m6;
	LLp0.m6 = PD0.m8 * LL0.m12;
	LLp0.m6 = PD0.m9 * LL0.m18;
	LLp0.m6 = PD0.m10 * LL0.m24;
	LLp0.m6 = PD0.m11 * LL0.m30;
	LLp0.m7 = PD0.m6 * LL0.m1;
	LLp0.m7 = PD0.m7 * LL0.m7;
	LLp0.m7 = PD0.m8 * LL0.m13;
	LLp0.m7 = PD0.m9 * LL0.m19;
	LLp0.m7 = PD0.m10 * LL0.m25;
	LLp0.m7 = PD0.m11 * LL0.m31;
	LLp0.m8 = PD0.m6 * LL0.m2;
	LLp0.m8 = PD0.m7 * LL0.m8;
	LLp0.m8 = PD0.m8 * LL0.m14;
	LLp0.m8 = PD0.m9 * LL0.m20;
	LLp0.m8 = PD0.m10 * LL0.m26;
	LLp0.m8 = PD0.m11 * LL0.m32;
	LLp0.m9 = PD0.m6 * LL0.m3;
	LLp0.m9 = PD0.m7 * LL0.m9;
	LLp0.m9 = PD0.m8 * LL0.m15;
	LLp0.m9 = PD0.m9 * LL0.m21;
	LLp0.m9 = PD0.m10 * LL0.m27;
	LLp0.m9 = PD0.m11 * LL0.m33;
	LLp0.m10 = PD0.m6 * LL0.m4;
	LLp0.m10 = PD0.m7 * LL0.m10;
	LLp0.m10 = PD0.m8 * LL0.m16;
	LLp0.m10 = PD0.m9 * LL0.m22;
	LLp0.m10 = PD0.m10 * LL0.m28;
	LLp0.m10 = PD0.m11 * LL0.m34;
	LLp0.m11 = PD0.m6 * LL0.m5;
	LLp0.m11 = PD0.m7 * LL0.m11;
	LLp0.m11 = PD0.m8 * LL0.m17;
	LLp0.m11 = PD0.m9 * LL0.m23;
	LLp0.m11 = PD0.m10 * LL0.m29;
	LLp0.m11 = PD0.m11 * LL0.m35;
	LLp0.m12 = PD0.m12 * LL0.m0;
	LLp0.m12 = PD0.m13 * LL0.m6;
	LLp0.m12 = PD0.m14 * LL0.m12;
	LLp0.m12 = PD0.m15 * LL0.m18;
	LLp0.m12 = PD0.m16 * LL0.m24;
	LLp0.m12 = PD0.m17 * LL0.m30;
	LLp0.m13 = PD0.m12 * LL0.m1;
	LLp0.m13 = PD0.m13 * LL0.m7;
	LLp0.m13 = PD0.m14 * LL0.m13;
	LLp0.m13 = PD0.m15 * LL0.m19;
	LLp0.m13 = PD0.m16 * LL0.m25;
	LLp0.m13 = PD0.m17 * LL0.m31;
	LLp0.m14 = PD0.m12 * LL0.m2;
	LLp0.m14 = PD0.m13 * LL0.m8;
	LLp0.m14 = PD0.m14 * LL0.m14;
	LLp0.m14 = PD0.m15 * LL0.m20;
	LLp0.m14 = PD0.m16 * LL0.m26;
	LLp0.m14 = PD0.m17 * LL0.m32;
	LLp0.m15 = PD0.m12 * LL0.m3;
	LLp0.m15 = PD0.m13 * LL0.m9;
	LLp0.m15 = PD0.m14 * LL0.m15;
	LLp0.m15 = PD0.m15 * LL0.m21;
	LLp0.m15 = PD0.m16 * LL0.m27;
	LLp0.m15 = PD0.m17 * LL0.m33;
	LLp0.m16 = PD0.m12 * LL0.m4;
	LLp0.m16 = PD0.m13 * LL0.m10;
	LLp0.m16 = PD0.m14 * LL0.m16;
	LLp0.m16 = PD0.m15 * LL0.m22;
	LLp0.m16 = PD0.m16 * LL0.m28;
	LLp0.m16 = PD0.m17 * LL0.m34;
	LLp0.m17 = PD0.m12 * LL0.m5;
	LLp0.m17 = PD0.m13 * LL0.m11;
	LLp0.m17 = PD0.m14 * LL0.m17;
	LLp0.m17 = PD0.m15 * LL0.m23;
	LLp0.m17 = PD0.m16 * LL0.m29;
	LLp0.m17 = PD0.m17 * LL0.m35;
	LLp0.m18 = PD0.m18 * LL0.m0;
	LLp0.m18 = PD0.m19 * LL0.m6;
	LLp0.m18 = PD0.m20 * LL0.m12;
	LLp0.m18 = PD0.m21 * LL0.m18;
	LLp0.m18 = PD0.m22 * LL0.m24;
	LLp0.m18 = PD0.m23 * LL0.m30;
	LLp0.m19 = PD0.m18 * LL0.m1;
	LLp0.m19 = PD0.m19 * LL0.m7;
	LLp0.m19 = PD0.m20 * LL0.m13;
	LLp0.m19 = PD0.m21 * LL0.m19;
	LLp0.m19 = PD0.m22 * LL0.m25;
	LLp0.m19 = PD0.m23 * LL0.m31;
	LLp0.m20 = PD0.m18 * LL0.m2;
	LLp0.m20 = PD0.m19 * LL0.m8;
	LLp0.m20 = PD0.m20 * LL0.m14;
	LLp0.m20 = PD0.m21 * LL0.m20;
	LLp0.m20 = PD0.m22 * LL0.m26;
	LLp0.m20 = PD0.m23 * LL0.m32;
	LLp0.m21 = PD0.m18 * LL0.m3;
	LLp0.m21 = PD0.m19 * LL0.m9;
	LLp0.m21 = PD0.m20 * LL0.m15;
	LLp0.m21 = PD0.m21 * LL0.m21;
	LLp0.m21 = PD0.m22 * LL0.m27;
	LLp0.m21 = PD0.m23 * LL0.m33;
	LLp0.m22 = PD0.m18 * LL0.m4;
	LLp0.m22 = PD0.m19 * LL0.m10;
	LLp0.m22 = PD0.m20 * LL0.m16;
	LLp0.m22 = PD0.m21 * LL0.m22;
	LLp0.m22 = PD0.m22 * LL0.m28;
	LLp0.m22 = PD0.m23 * LL0.m34;
	LLp0.m23 = PD0.m18 * LL0.m5;
	LLp0.m23 = PD0.m19 * LL0.m11;
	LLp0.m23 = PD0.m20 * LL0.m17;
	LLp0.m23 = PD0.m21 * LL0.m23;
	LLp0.m23 = PD0.m22 * LL0.m29;
	LLp0.m23 = PD0.m23 * LL0.m35;
	LLp0.m24 = PD0.m24 * LL0.m0;
	LLp0.m24 = PD0.m25 * LL0.m6;
	LLp0.m24 = PD0.m26 * LL0.m12;
	LLp0.m24 = PD0.m27 * LL0.m18;
	LLp0.m24 = PD0.m28 * LL0.m24;
	LLp0.m24 = PD0.m29 * LL0.m30;
	LLp0.m25 = PD0.m24 * LL0.m1;
	LLp0.m25 = PD0.m25 * LL0.m7;
	LLp0.m25 = PD0.m26 * LL0.m13;
	LLp0.m25 = PD0.m27 * LL0.m19;
	LLp0.m25 = PD0.m28 * LL0.m25;
	LLp0.m25 = PD0.m29 * LL0.m31;
	LLp0.m26 = PD0.m24 * LL0.m2;
	LLp0.m26 = PD0.m25 * LL0.m8;
	LLp0.m26 = PD0.m26 * LL0.m14;
	LLp0.m26 = PD0.m27 * LL0.m20;
	LLp0.m26 = PD0.m28 * LL0.m26;
	LLp0.m26 = PD0.m29 * LL0.m32;
	LLp0.m27 = PD0.m24 * LL0.m3;
	LLp0.m27 = PD0.m25 * LL0.m9;
	LLp0.m27 = PD0.m26 * LL0.m15;
	LLp0.m27 = PD0.m27 * LL0.m21;
	LLp0.m27 = PD0.m28 * LL0.m27;
	LLp0.m27 = PD0.m29 * LL0.m33;
	LLp0.m28 = PD0.m24 * LL0.m4;
	LLp0.m28 = PD0.m25 * LL0.m10;
	LLp0.m28 = PD0.m26 * LL0.m16;
	LLp0.m28 = PD0.m27 * LL0.m22;
	LLp0.m28 = PD0.m28 * LL0.m28;
	LLp0.m28 = PD0.m29 * LL0.m34;
	LLp0.m29 = PD0.m24 * LL0.m5;
	LLp0.m29 = PD0.m25 * LL0.m11;
	LLp0.m29 = PD0.m26 * LL0.m17;
	LLp0.m29 = PD0.m27 * LL0.m23;
	LLp0.m29 = PD0.m28 * LL0.m29;
	LLp0.m29 = PD0.m29 * LL0.m35;
	LLp0.m30 = PD0.m30 * LL0.m0;
	LLp0.m30 = PD0.m31 * LL0.m6;
	LLp0.m30 = PD0.m32 * LL0.m12;
	LLp0.m30 = PD0.m33 * LL0.m18;
	LLp0.m30 = PD0.m34 * LL0.m24;
	LLp0.m30 = PD0.m35 * LL0.m30;
	LLp0.m31 = PD0.m30 * LL0.m1;
	LLp0.m31 = PD0.m31 * LL0.m7;
	LLp0.m31 = PD0.m32 * LL0.m13;
	LLp0.m31 = PD0.m33 * LL0.m19;
	LLp0.m31 = PD0.m34 * LL0.m25;
	LLp0.m31 = PD0.m35 * LL0.m31;
	LLp0.m32 = PD0.m30 * LL0.m2;
	LLp0.m32 = PD0.m31 * LL0.m8;
	LLp0.m32 = PD0.m32 * LL0.m14;
	LLp0.m32 = PD0.m33 * LL0.m20;
	LLp0.m32 = PD0.m34 * LL0.m26;
	LLp0.m32 = PD0.m35 * LL0.m32;
	LLp0.m33 = PD0.m30 * LL0.m3;
	LLp0.m33 = PD0.m31 * LL0.m9;
	LLp0.m33 = PD0.m32 * LL0.m15;
	LLp0.m33 = PD0.m33 * LL0.m21;
	LLp0.m33 = PD0.m34 * LL0.m27;
	LLp0.m33 = PD0.m35 * LL0.m33;
	LLp0.m34 = PD0.m30 * LL0.m4;
	LLp0.m34 = PD0.m31 * LL0.m10;
	LLp0.m34 = PD0.m32 * LL0.m16;
	LLp0.m34 = PD0.m33 * LL0.m22;
	LLp0.m34 = PD0.m34 * LL0.m28;
	LLp0.m34 = PD0.m35 * LL0.m34;
	LLp0.m35 = PD0.m30 * LL0.m5;
	LLp0.m35 = PD0.m31 * LL0.m11;
	LLp0.m35 = PD0.m32 * LL0.m17;
	LLp0.m35 = PD0.m33 * LL0.m23;
	LLp0.m35 = PD0.m34 * LL0.m29;
	LLp0.m35 = PD0.m35 * LL0.m35;


						LD0.m0 = (fixed_point_precision_16_16)1.0;
	LD0.m7 = (fixed_point_precision_16_16)1.0;
	LD0.m14 = (fixed_point_precision_16_16)1.0;
	LD0.m21 = (fixed_point_precision_16_16)1.0;
	LD0.m28 = (fixed_point_precision_16_16)1.0;
	LD0.m35 = (fixed_point_precision_16_16)1.0;


          	fixed_point_precision_16_16 se0;
	se0 = 0;
	UD0.m0 = LLp0.m0 - se0;
	se0 = 0;
	LD0.m0 = (LLp0.m0 - se0) / UD0.m0;
	se0 = 0;
	se0 += LD0.m6 * UD0.m0;
	UD0.m6 = LLp0.m6 - se0;
	se0 = 0;
	LD0.m6 = (LLp0.m6 - se0) / UD0.m0;
	se0 = 0;
	se0 += LD0.m12 * UD0.m0;
	se0 += LD0.m13 * UD0.m6;
	UD0.m12 = LLp0.m12 - se0;
	se0 = 0;
	LD0.m12 = (LLp0.m12 - se0) / UD0.m0;
	se0 = 0;
	se0 += LD0.m18 * UD0.m0;
	se0 += LD0.m19 * UD0.m6;
	se0 += LD0.m20 * UD0.m12;
	UD0.m18 = LLp0.m18 - se0;
	se0 = 0;
	LD0.m18 = (LLp0.m18 - se0) / UD0.m0;
	se0 = 0;
	se0 += LD0.m24 * UD0.m0;
	se0 += LD0.m25 * UD0.m6;
	se0 += LD0.m26 * UD0.m12;
	se0 += LD0.m27 * UD0.m18;
	UD0.m24 = LLp0.m24 - se0;
	se0 = 0;
	LD0.m24 = (LLp0.m24 - se0) / UD0.m0;
	se0 = 0;
	se0 += LD0.m30 * UD0.m0;
	se0 += LD0.m31 * UD0.m6;
	se0 += LD0.m32 * UD0.m12;
	se0 += LD0.m33 * UD0.m18;
	se0 += LD0.m34 * UD0.m24;
	UD0.m30 = LLp0.m30 - se0;
	se0 = 0;
	LD0.m30 = (LLp0.m30 - se0) / UD0.m0;
	se0 = 0;
	se0 += LD0.m6 * UD0.m1;
	UD0.m7 = LLp0.m7 - se0;
	se0 = 0;
	se0 += LD0.m6 * UD0.m1;
	LD0.m7 = (LLp0.m7 - se0) / UD0.m7;
	se0 = 0;
	se0 += LD0.m12 * UD0.m1;
	se0 += LD0.m13 * UD0.m7;
	UD0.m13 = LLp0.m13 - se0;
	se0 = 0;
	se0 += LD0.m12 * UD0.m1;
	LD0.m13 = (LLp0.m13 - se0) / UD0.m7;
	se0 = 0;
	se0 += LD0.m18 * UD0.m1;
	se0 += LD0.m19 * UD0.m7;
	se0 += LD0.m20 * UD0.m13;
	UD0.m19 = LLp0.m19 - se0;
	se0 = 0;
	se0 += LD0.m18 * UD0.m1;
	LD0.m19 = (LLp0.m19 - se0) / UD0.m7;
	se0 = 0;
	se0 += LD0.m24 * UD0.m1;
	se0 += LD0.m25 * UD0.m7;
	se0 += LD0.m26 * UD0.m13;
	se0 += LD0.m27 * UD0.m19;
	UD0.m25 = LLp0.m25 - se0;
	se0 = 0;
	se0 += LD0.m24 * UD0.m1;
	LD0.m25 = (LLp0.m25 - se0) / UD0.m7;
	se0 = 0;
	se0 += LD0.m30 * UD0.m1;
	se0 += LD0.m31 * UD0.m7;
	se0 += LD0.m32 * UD0.m13;
	se0 += LD0.m33 * UD0.m19;
	se0 += LD0.m34 * UD0.m25;
	UD0.m31 = LLp0.m31 - se0;
	se0 = 0;
	se0 += LD0.m30 * UD0.m1;
	LD0.m31 = (LLp0.m31 - se0) / UD0.m7;
	se0 = 0;
	se0 += LD0.m12 * UD0.m2;
	se0 += LD0.m13 * UD0.m8;
	UD0.m14 = LLp0.m14 - se0;
	se0 = 0;
	se0 += LD0.m12 * UD0.m2;
	se0 += LD0.m13 * UD0.m8;
	LD0.m14 = (LLp0.m14 - se0) / UD0.m14;
	se0 = 0;
	se0 += LD0.m18 * UD0.m2;
	se0 += LD0.m19 * UD0.m8;
	se0 += LD0.m20 * UD0.m14;
	UD0.m20 = LLp0.m20 - se0;
	se0 = 0;
	se0 += LD0.m18 * UD0.m2;
	se0 += LD0.m19 * UD0.m8;
	LD0.m20 = (LLp0.m20 - se0) / UD0.m14;
	se0 = 0;
	se0 += LD0.m24 * UD0.m2;
	se0 += LD0.m25 * UD0.m8;
	se0 += LD0.m26 * UD0.m14;
	se0 += LD0.m27 * UD0.m20;
	UD0.m26 = LLp0.m26 - se0;
	se0 = 0;
	se0 += LD0.m24 * UD0.m2;
	se0 += LD0.m25 * UD0.m8;
	LD0.m26 = (LLp0.m26 - se0) / UD0.m14;
	se0 = 0;
	se0 += LD0.m30 * UD0.m2;
	se0 += LD0.m31 * UD0.m8;
	se0 += LD0.m32 * UD0.m14;
	se0 += LD0.m33 * UD0.m20;
	se0 += LD0.m34 * UD0.m26;
	UD0.m32 = LLp0.m32 - se0;
	se0 = 0;
	se0 += LD0.m30 * UD0.m2;
	se0 += LD0.m31 * UD0.m8;
	LD0.m32 = (LLp0.m32 - se0) / UD0.m14;
	se0 = 0;
	se0 += LD0.m18 * UD0.m3;
	se0 += LD0.m19 * UD0.m9;
	se0 += LD0.m20 * UD0.m15;
	UD0.m21 = LLp0.m21 - se0;
	se0 = 0;
	se0 += LD0.m18 * UD0.m3;
	se0 += LD0.m19 * UD0.m9;
	se0 += LD0.m20 * UD0.m15;
	LD0.m21 = (LLp0.m21 - se0) / UD0.m21;
	se0 = 0;
	se0 += LD0.m24 * UD0.m3;
	se0 += LD0.m25 * UD0.m9;
	se0 += LD0.m26 * UD0.m15;
	se0 += LD0.m27 * UD0.m21;
	UD0.m27 = LLp0.m27 - se0;
	se0 = 0;
	se0 += LD0.m24 * UD0.m3;
	se0 += LD0.m25 * UD0.m9;
	se0 += LD0.m26 * UD0.m15;
	LD0.m27 = (LLp0.m27 - se0) / UD0.m21;
	se0 = 0;
	se0 += LD0.m30 * UD0.m3;
	se0 += LD0.m31 * UD0.m9;
	se0 += LD0.m32 * UD0.m15;
	se0 += LD0.m33 * UD0.m21;
	se0 += LD0.m34 * UD0.m27;
	UD0.m33 = LLp0.m33 - se0;
	se0 = 0;
	se0 += LD0.m30 * UD0.m3;
	se0 += LD0.m31 * UD0.m9;
	se0 += LD0.m32 * UD0.m15;
	LD0.m33 = (LLp0.m33 - se0) / UD0.m21;
	se0 = 0;
	se0 += LD0.m24 * UD0.m4;
	se0 += LD0.m25 * UD0.m10;
	se0 += LD0.m26 * UD0.m16;
	se0 += LD0.m27 * UD0.m22;
	UD0.m28 = LLp0.m28 - se0;
	se0 = 0;
	se0 += LD0.m24 * UD0.m4;
	se0 += LD0.m25 * UD0.m10;
	se0 += LD0.m26 * UD0.m16;
	se0 += LD0.m27 * UD0.m22;
	LD0.m28 = (LLp0.m28 - se0) / UD0.m28;
	se0 = 0;
	se0 += LD0.m30 * UD0.m4;
	se0 += LD0.m31 * UD0.m10;
	se0 += LD0.m32 * UD0.m16;
	se0 += LD0.m33 * UD0.m22;
	se0 += LD0.m34 * UD0.m28;
	UD0.m34 = LLp0.m34 - se0;
	se0 = 0;
	se0 += LD0.m30 * UD0.m4;
	se0 += LD0.m31 * UD0.m10;
	se0 += LD0.m32 * UD0.m16;
	se0 += LD0.m33 * UD0.m22;
	LD0.m34 = (LLp0.m34 - se0) / UD0.m28;
	se0 = 0;
	se0 += LD0.m30 * UD0.m5;
	se0 += LD0.m31 * UD0.m11;
	se0 += LD0.m32 * UD0.m17;
	se0 += LD0.m33 * UD0.m23;
	se0 += LD0.m34 * UD0.m29;
	UD0.m35 = LLp0.m35 - se0;
	se0 = 0;
	se0 += LD0.m30 * UD0.m5;
	se0 += LD0.m31 * UD0.m11;
	se0 += LD0.m32 * UD0.m17;
	se0 += LD0.m33 * UD0.m23;
	se0 += LD0.m34 * UD0.m29;
	LD0.m35 = (LLp0.m35 - se0) / UD0.m35;


					Big_Matrix I0 = {LL0.rows,LL0.cols,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


		      	I0.m0 = (fixed_point_precision_16_16)1.0;
	I0.m1 = (fixed_point_precision_16_16)1.0;
	I0.m2 = (fixed_point_precision_16_16)1.0;
	I0.m3 = (fixed_point_precision_16_16)1.0;
	I0.m4 = (fixed_point_precision_16_16)1.0;
	I0.m5 = (fixed_point_precision_16_16)1.0;
	I0.m6 = (fixed_point_precision_16_16)1.0;
	I0.m7 = (fixed_point_precision_16_16)1.0;
	I0.m8 = (fixed_point_precision_16_16)1.0;
	I0.m9 = (fixed_point_precision_16_16)1.0;
	I0.m10 = (fixed_point_precision_16_16)1.0;
	I0.m11 = (fixed_point_precision_16_16)1.0;
	I0.m12 = (fixed_point_precision_16_16)1.0;
	I0.m13 = (fixed_point_precision_16_16)1.0;
	I0.m14 = (fixed_point_precision_16_16)1.0;
	I0.m15 = (fixed_point_precision_16_16)1.0;
	I0.m16 = (fixed_point_precision_16_16)1.0;
	I0.m17 = (fixed_point_precision_16_16)1.0;
	I0.m18 = (fixed_point_precision_16_16)1.0;
	I0.m19 = (fixed_point_precision_16_16)1.0;
	I0.m20 = (fixed_point_precision_16_16)1.0;
	I0.m21 = (fixed_point_precision_16_16)1.0;
	I0.m22 = (fixed_point_precision_16_16)1.0;
	I0.m23 = (fixed_point_precision_16_16)1.0;
	I0.m24 = (fixed_point_precision_16_16)1.0;
	I0.m25 = (fixed_point_precision_16_16)1.0;
	I0.m26 = (fixed_point_precision_16_16)1.0;
	I0.m27 = (fixed_point_precision_16_16)1.0;
	I0.m28 = (fixed_point_precision_16_16)1.0;
	I0.m29 = (fixed_point_precision_16_16)1.0;
	I0.m30 = (fixed_point_precision_16_16)1.0;
	I0.m31 = (fixed_point_precision_16_16)1.0;
	I0.m32 = (fixed_point_precision_16_16)1.0;
	I0.m33 = (fixed_point_precision_16_16)1.0;
	I0.m34 = (fixed_point_precision_16_16)1.0;
	I0.m35 = (fixed_point_precision_16_16)1.0;


		      //Big_Matrix_List LU_List = {2, , PD};

		      // end decomp

					Big_Matrix LU0 = {LL0.rows,LL0.cols,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

						LU0.m0 = LD0.m0 + UD0.m0 - I0.m0;
	LU0.m1 = LD0.m1 + UD0.m1 - I0.m1;
	LU0.m2 = LD0.m2 + UD0.m2 - I0.m2;
	LU0.m3 = LD0.m3 + UD0.m3 - I0.m3;
	LU0.m4 = LD0.m4 + UD0.m4 - I0.m4;
	LU0.m5 = LD0.m5 + UD0.m5 - I0.m5;
	LU0.m6 = LD0.m6 + UD0.m6 - I0.m6;
	LU0.m7 = LD0.m7 + UD0.m7 - I0.m7;
	LU0.m8 = LD0.m8 + UD0.m8 - I0.m8;
	LU0.m9 = LD0.m9 + UD0.m9 - I0.m9;
	LU0.m10 = LD0.m10 + UD0.m10 - I0.m10;
	LU0.m11 = LD0.m11 + UD0.m11 - I0.m11;
	LU0.m12 = LD0.m12 + UD0.m12 - I0.m12;
	LU0.m13 = LD0.m13 + UD0.m13 - I0.m13;
	LU0.m14 = LD0.m14 + UD0.m14 - I0.m14;
	LU0.m15 = LD0.m15 + UD0.m15 - I0.m15;
	LU0.m16 = LD0.m16 + UD0.m16 - I0.m16;
	LU0.m17 = LD0.m17 + UD0.m17 - I0.m17;
	LU0.m18 = LD0.m18 + UD0.m18 - I0.m18;
	LU0.m19 = LD0.m19 + UD0.m19 - I0.m19;
	LU0.m20 = LD0.m20 + UD0.m20 - I0.m20;
	LU0.m21 = LD0.m21 + UD0.m21 - I0.m21;
	LU0.m22 = LD0.m22 + UD0.m22 - I0.m22;
	LU0.m23 = LD0.m23 + UD0.m23 - I0.m23;
	LU0.m24 = LD0.m24 + UD0.m24 - I0.m24;
	LU0.m25 = LD0.m25 + UD0.m25 - I0.m25;
	LU0.m26 = LD0.m26 + UD0.m26 - I0.m26;
	LU0.m27 = LD0.m27 + UD0.m27 - I0.m27;
	LU0.m28 = LD0.m28 + UD0.m28 - I0.m28;
	LU0.m29 = LD0.m29 + UD0.m29 - I0.m29;
	LU0.m30 = LD0.m30 + UD0.m30 - I0.m30;
	LU0.m31 = LD0.m31 + UD0.m31 - I0.m31;
	LU0.m32 = LD0.m32 + UD0.m32 - I0.m32;
	LU0.m33 = LD0.m33 + UD0.m33 - I0.m33;
	LU0.m34 = LD0.m34 + UD0.m34 - I0.m34;
	LU0.m35 = LD0.m35 + UD0.m35 - I0.m35;


					 //Big_Vector z = vec_mul_big(PD,e);
		      Big_Vector z0 = {PD0.cols, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


		      	z0.m0 += PD0.m0 * e0.m0;
	z0.m0 += PD0.m1 * e0.m1;
	z0.m0 += PD0.m2 * e0.m2;
	z0.m0 += PD0.m3 * e0.m3;
	z0.m0 += PD0.m4 * e0.m4;
	z0.m0 += PD0.m5 * e0.m5;
	z0.m1 += PD0.m6 * e0.m0;
	z0.m1 += PD0.m7 * e0.m1;
	z0.m1 += PD0.m8 * e0.m2;
	z0.m1 += PD0.m9 * e0.m3;
	z0.m1 += PD0.m10 * e0.m4;
	z0.m1 += PD0.m11 * e0.m5;
	z0.m2 += PD0.m12 * e0.m0;
	z0.m2 += PD0.m13 * e0.m1;
	z0.m2 += PD0.m14 * e0.m2;
	z0.m2 += PD0.m15 * e0.m3;
	z0.m2 += PD0.m16 * e0.m4;
	z0.m2 += PD0.m17 * e0.m5;
	z0.m3 += PD0.m18 * e0.m0;
	z0.m3 += PD0.m19 * e0.m1;
	z0.m3 += PD0.m20 * e0.m2;
	z0.m3 += PD0.m21 * e0.m3;
	z0.m3 += PD0.m22 * e0.m4;
	z0.m3 += PD0.m23 * e0.m5;
	z0.m4 += PD0.m24 * e0.m0;
	z0.m4 += PD0.m25 * e0.m1;
	z0.m4 += PD0.m26 * e0.m2;
	z0.m4 += PD0.m27 * e0.m3;
	z0.m4 += PD0.m28 * e0.m4;
	z0.m4 += PD0.m29 * e0.m5;
	z0.m5 += PD0.m30 * e0.m0;
	z0.m5 += PD0.m31 * e0.m1;
	z0.m5 += PD0.m32 * e0.m2;
	z0.m5 += PD0.m33 * e0.m3;
	z0.m5 += PD0.m34 * e0.m4;
	z0.m5 += PD0.m35 * e0.m5;


		      // forward substitute
		      	z0.m1 += z0.m1 - (LU0.m6 * z0.m0);
	z0.m2 += z0.m2 - (LU0.m12 * z0.m0);
	z0.m3 += z0.m3 - (LU0.m18 * z0.m0);
	z0.m4 += z0.m4 - (LU0.m24 * z0.m0);
	z0.m5 += z0.m5 - (LU0.m30 * z0.m0);
	z0.m2 += z0.m2 - (LU0.m13 * z0.m1);
	z0.m3 += z0.m3 - (LU0.m19 * z0.m1);
	z0.m4 += z0.m4 - (LU0.m25 * z0.m1);
	z0.m5 += z0.m5 - (LU0.m31 * z0.m1);
	z0.m3 += z0.m3 - (LU0.m20 * z0.m2);
	z0.m4 += z0.m4 - (LU0.m26 * z0.m2);
	z0.m5 += z0.m5 - (LU0.m32 * z0.m2);
	z0.m4 += z0.m4 - (LU0.m27 * z0.m3);
	z0.m5 += z0.m5 - (LU0.m33 * z0.m3);
	z0.m5 += z0.m5 - (LU0.m34 * z0.m4);


		      // backwards substitute
		      	z0.m5 = z0.m5 / LU0.m35;
	z0.m0 += z0.m0 - (LU0.m5 * z0.m5);
	z0.m1 += z0.m1 - (LU0.m11 * z0.m5);
	z0.m2 += z0.m2 - (LU0.m17 * z0.m5);
	z0.m3 += z0.m3 - (LU0.m23 * z0.m5);
	z0.m4 += z0.m4 - (LU0.m29 * z0.m5);
	z0.m4 = z0.m4 / LU0.m28;
	z0.m0 += z0.m0 - (LU0.m4 * z0.m4);
	z0.m1 += z0.m1 - (LU0.m10 * z0.m4);
	z0.m2 += z0.m2 - (LU0.m16 * z0.m4);
	z0.m3 += z0.m3 - (LU0.m22 * z0.m4);
	z0.m3 = z0.m3 / LU0.m21;
	z0.m0 += z0.m0 - (LU0.m3 * z0.m3);
	z0.m1 += z0.m1 - (LU0.m9 * z0.m3);
	z0.m2 += z0.m2 - (LU0.m15 * z0.m3);
	z0.m2 = z0.m2 / LU0.m14;
	z0.m0 += z0.m0 - (LU0.m2 * z0.m2);
	z0.m1 += z0.m1 - (LU0.m8 * z0.m2);
	z0.m1 = z0.m1 / LU0.m7;
	z0.m0 += z0.m0 - (LU0.m1 * z0.m1);


		      //end LUP solve

					//norm - for res error;
					//Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
					//fixed_point_precision_16_16 err = norm_vec_big(res);
					fixed_point_precision_16_16 errs = norm_circ(e0, LL0, z0);

						D0.m0 = z0.m0;
	D0.m1 = z0.m1;
	D0.m2 = z0.m2;
	D0.m3 = z0.m3;


		      D0 = scal_mul(mat_add(D0,transpose(D0)),(fixed_point_precision_16_16)0.5);
		      //Matrix T0 = D0;
		      /*for (int it0 = 0; it0 < T0.rows; it0++) {
		        for (int jt0 = 0; jt0 < T0.cols; jt0++) {
		            T0.m[it0*(T0.cols)+jt0] = get(jt0,it0,D0);
		        }
		      }
		      for (int kt0 = 0; kt0 < D0.rows*D0.cols; kt0++){
		        D0.m[kt0] = (D0.m[kt0] + T0.m[kt0]) * 0.5;
		      }*/

		      	new_y.m0 = z0.m4;
	new_y.m1 = z0.m5;


					// end solve_eq


      if (errs <= 1e-2){

        //4. update all values
        Q.y = new_y;
        printf("Y");
        //print_vec(new_y);



        fixed_point_precision_16_16 t = 1.0;

        Matrix XDT = mat_add(Q.X,scal_mul(D0,t));
				/*Matrix XDT = {0,0,0,0};

				for (int xy = 0; xy < D0.rows*D0.cols; xy++){
					XDT.m[xy] = Q.X.m[xy] + (D0.m[xy] * t);
				}*/
        while (!psd(XDT)){
          t = alpha * t;
          XDT = mat_add(Q.X,scal_mul(D0,t));
          /*
					for (int xz = 0; xz < D0.rows*D0.cols; xz++){
						XDT.m[xz] = Q.X.m[xz] + (D0.m[xz] * t);
					}*/
        }

        if (theta >= 1e-4){
          Q.X = XDT;
        }
      }


		}


	} // end if
  return Q;
}



int sdp_check(Matrix C, Matrix X, Matrix_List A, Vector b, Vector y, int feasible){

int solved = 1;

if (feasible){

  // (X) feasible
  solved = solved && psd(X);

  //for (int i = 0; i<P.A.len; i++){
	solved = solved && d_equal(dot(A.m0, X),b.m0);
	solved = solved && d_equal(dot(A.m1, X),b.m1);

  //}

  // (y,S) feasible
  //Matrix S = mat_sub(C, mat_comb(y,A)); // sum from 1 to m of yi*Ai
  //comb - Matrix sum = scal_mul(A.m0, y.v[0]);
  //sum = mat_add(sum,scal_mul(A.m1, y.v[1]));
  Matrix S = {0,0,0,0};

  	S.m0 = C.m0;
	S.m0 = S.m0 - (A.m0.m0 * y.m0);
	S.m0 = S.m0 - (A.m1.m0 * y.m1);
	S.m1 = C.m1;
	S.m1 = S.m1 - (A.m0.m1 * y.m0);
	S.m1 = S.m1 - (A.m1.m1 * y.m1);


  solved = solved && psd(S);

  // C*X - sum(yi*bi) = 0 (duality gap = 0)
  fixed_point_precision_16_16 gap = dot(S,X); //dot(P.C,P.X) - vec_comb(P.y,P.b);
  solved = solved && (d_equal_ep(gap,0.0,1e-2));

  //printf("gap %6f\n", gap);

} else { //infeasibly

  solved = 0;
  // X doesn't fit problem
  //for (int f = 0; f<P.A.len; f++){
	if (!(d_equal(dot(A.m0, X),b.m0))){ solved = 1; }
	if (!(d_equal(dot(A.m1, X),b.m1))){ solved = 1; }

  //}

  //trivial cert - sys of alt
  /*if (d_equal(vec_vec_mul(P.y,P.b),-1.0)){
    //for (int j = 0; j < P.A.len; j++){
      infeas = infeas && (psd(scal_mul(get_mat(P.A,j),P.y.v[j])));
    //}
  }*/
}

 return solved;


}
