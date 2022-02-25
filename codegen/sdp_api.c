#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "sdp_api.h"
/*
int check_sdp2(int n,int m,double c0,double c1,double c2,double c3,double c4,double c5,double c6,double c7,double c8,double x0,double x1,double x2,double x3,double x4,double x5,double x6,double x7,double x8,double a0_0,double a0_1,double a0_2,double a0_3,double a0_4,double a0_5,double a0_6,double a0_7,double a0_8,double a1_0,double a1_1,double a1_2,double a1_3,double a1_4,double a1_5,double a1_6,double a1_7,double a1_8,double b0,double b1,double y0,double y1){

  int solved = 1;

  if (1){ // x feasible

solved = solved && (x0 > -0.01);
solved = solved && (x4 > -0.01);
solved = solved && (x8 > -0.01);
double dx_r2_c2 = (x0*x4) - (x1*x3);
solved = solved && (dx_r2_c2 > -0.01);
double dx_r1_c1 = (x0*x8) - (x2*x6);
solved = solved && (dx_r1_c1 > -0.01);
double dx_r0_c0 = (x4*x8) - (x5*x7);
solved = solved && (dx_r0_c0 > -0.01);
double dx_r0_c1 = (x3*x8) - (x5*x6);
double dx_r0_c2 = (x3*x7) - (x4*x6);
double dx = (x0 * dx_r0_c0) + (x1 * dx_r0_c1 * -1) + (x2 * dx_r0_c2);
solved = solved && (dx > -0.01);



double dot_s0f = (a0_0*x0) + (a0_1*x1) + (a0_2*x2) + (a0_3*x3) + (a0_4*x4) + (a0_5*x5) + (a0_6*x6) + (a0_7*x7) + (a0_8*x8);
double dot_s1f = (a1_0*x0) + (a1_1*x1) + (a1_2*x2) + (a1_3*x3) + (a1_4*x4) + (a1_5*x5) + (a1_6*x6) + (a1_7*x7) + (a1_8*x8);


double s0 = c0 - ((a0_0 * y0) + (a1_0 * y1));
double s1 = c1 - ((a0_1 * y0) + (a1_1 * y1));
double s2 = c2 - ((a0_2 * y0) + (a1_2 * y1));
double s3 = c3 - ((a0_3 * y0) + (a1_3 * y1));
double s4 = c4 - ((a0_4 * y0) + (a1_4 * y1));
double s5 = c5 - ((a0_5 * y0) + (a1_5 * y1));
double s6 = c6 - ((a0_6 * y0) + (a1_6 * y1));
double s7 = c7 - ((a0_7 * y0) + (a1_7 * y1));
double s8 = c8 - ((a0_8 * y0) + (a1_8 * y1));


solved = solved && (s0 > -0.01);
solved = solved && (s4 > -0.01);
solved = solved && (s8 > -0.01);
double ds_r2_c2 = (s0*s4) - (s1*s3);
printf("BREAK point %f\n", ds_r2_c2);
solved = solved && (ds_r2_c2 > -0.01);
double ds_r1_c1 = (s0*s8) - (s2*s6);
printf("BREAK point %f\n", ds_r1_c1);
solved = solved && (ds_r1_c1 > -0.01);
double ds_r0_c0 = (s4*s8) - (s5*s7);
solved = solved && (ds_r0_c0 > -0.01);
double ds_r0_c1 = (s3*s8) - (s5*s6);
double ds_r0_c2 = (s3*s7) - (s4*s6);
double ds = (s0 * ds_r0_c0) + (s1 * ds_r0_c1 * -1) + (s2 * ds_r0_c2);
solved = solved && (ds > -0.01);



double gap = (s0*x0) + (s1*x1) + (s2*x2) + (s3*x3) + (s4*x4) + (s5*x5) + (s6*x6) + (s7*x7) + (s8*x8);

solved = solved && (d_equal(gap,0.0));

}

return solved;

}

int main(void){

		printf("START\n");

	  // problem

	  double C[9] = {-0.99154214,  0.65386878, -0.64033738, 0.65386878,  0.93792596, -0.14210919, -0.64033738, -0.14210919,  0.30795382};
	  double b[9] = {3.9888687923835366, -1.0823059466682765};
	  double X[9] = {1.78032993, -0.00358676,  0.72814533, -0.00358676,  1.22372938, -0.05358303, 0.72814533, -0.05358303,  1.84385819};

	  double A0[9] = {0.35184943, -0.38338125,  0.31362847, -0.38338125,  0.85702573, -0.58909452, 0.31362847, -0.58909452,  0.97137504};
	  double A1[9] = {-0.4945009 , -0.62078468,  0.20384815, -0.62078468,  0.21578012, -0.09717294, 0.20384815, -0.09717294, -0.42178768};

    double A[18] = {0.35184943, -0.38338125,  0.31362847, -0.38338125,  0.85702573, -0.58909452, 0.31362847, -0.58909452,  0.97137504, -0.4945009 , -0.62078468,  0.20384815, -0.62078468,  0.21578012, -0.09717294, 0.20384815, -0.09717294, -0.42178768};

    //Matrix_List A = {M,A0,A1};


  double *sol = malloc(30 * sizeof(double));


	sdp_solve(3,2,C,X,A,b,sol);

    //printf("\n\n");
    for (int i = 0; i < 9; i++){
       printf("x - %6.3f\n", sol[i]);
    }

		int check = check_sdp2(3,2,-0.99154214,0.65386878,-0.64033738,0.65386878,0.93792596,-0.14210919,-0.64033738,-0.14210919,0.30795382,sol[0],sol[1],sol[2],sol[3],sol[4],sol[5],sol[6],sol[7],sol[8],0.35184943,-0.38338125,0.31362847,-0.38338125,0.85702573,-0.58909452,0.31362847,-0.58909452,0.97137504,-0.4945009,-0.62078468,0.20384815,-0.62078468,0.21578012,-0.09717294,0.20384815,-0.09717294,-0.42178768,3.9888687923835366,-1.0823059466682765,sol[9],sol[10]);
		printf("CHECK = %d\n", check);
}
*/


Matrix *create_matrix(int rows, int cols, double *a){
	Matrix *mat = (Matrix *)malloc(sizeof(Matrix));
	mat->rows = rows;
	mat->cols = cols;
	if (a == NULL){
		mat->m = (double *)malloc(sizeof(double) * rows*cols);
		for (int i = 0; i < rows*cols; i++){
			mat->m[i] = 0.0;
		}
	} else {
		mat->m = a;
	}

	return mat;
}

void free_matrix(Matrix *mat){
	free(mat->m);
	free(mat);
}

Vector *create_vector(int l, double *a){
	Vector *vec = (Vector *)malloc(sizeof(Vector));
	vec->len = l;
	if (a == NULL){
		vec->v = (double *)malloc(sizeof(double) * l);
		for (int i = 0; i < l; i++){
			vec->v[i] = 0.0;
		}
	} else {
		vec->v = a;
	}

	return vec;
}

void free_vector(Vector *vec){
	free(vec->v);
	free(vec);
}

Matrix_List *create_matrix_list(int m, int n, double *a){
	Matrix_List *list = (Matrix_List *)malloc(sizeof(Matrix_List));
	list->len = m;
	list->m = (Matrix **)malloc(sizeof(Matrix *) * m);
	for (int i = 0; i < m; i++){
		list->m[i] = create_matrix(n, n, NULL);
	}
	if (a != NULL){
		for (int i = 0; i < m; i++){
			for (int j = 0; j < n*n; j++){
				list->m[i]->m[j] = *a;
				a++;
			}
		}
	}

	return list;
}


// N, M, C, X, big array of A's, b, y, feasible
void sdp_solve(int n, int m, double *c, double *x, double *a, double *b, double *sol){

    Matrix *C = create_matrix(n,n,c);
    Vector *B = create_vector(m,b);
    Matrix *X = create_matrix(n,n,x);
		/*
    for (int i = 0; i < n*n; i++){
      C->m[i] = *c;
      c++;
      X->m[i] = *x;
      x++;
    }

    for (int i = 0; i < m; i++){
      B->v[i] = *b;
      b++;
    }
		*/


    Matrix_List *A = create_matrix_list(m,n,a);

/*
    //printf("As\n");
    //print_mat(A0);
    //print_mat(A1);
*/

    Solution *Q = sdp(n,m,C,X,A,B,1.0,0.25);

    //x = Q->X->m;
    //y = Q->y->v;

    //double *sol = malloc(n*n+m * sizeof(double));

    for (int i = 0; i < n*n; i++){
      sol[i] = Q->X->m[i];
    }

    for (int i = 0; i < m; i++){
       sol[(n*n)+i] = Q->y->v[i];
     }

/*
    //printf("SOL\n\n");
    //print_mat(Q->X);

    for (int i = 0; i < n*n; i++){
      //printf("x - %6.3f\n", sol[i]);
    }
*/


  }





  double get(int i, int j, Matrix *mat){
	  return mat->m[i*(mat->cols)+j];
	}

	int d_equal(double a, double b) {
	  if ((a-b) > 0) {
	    return (a-b) < epsilon;
	  } else {
	    return -1*(a-b) < epsilon;
	  }
	}

	int d_equal_ep(double a, double b,double ep){
	  if ((a-b) > 0) {
	    return (a-b) < ep;
	  } else {
	    return -1*(a-b) < ep;
	  }
	}


	void print_mat(Matrix *X) {

	  printf("\n-----------------\n");

	  for(int i=0;i<X->rows; i++) {
	    for(int j=0;j<X->cols; j++) {
	      if (d_equal((int)(get(i,j,X)), (get(i,j,X)))) {
		printf(" %6d", (int)get(i,j,X));
	      } else {
		printf(" %6.3f", get(i,j,X));
	      }
	    }
	    printf("\n");

	  }

	  printf("\n----------------\n");
	}

	void print_vec(Vector *v) {

	  printf("\n-----------------\n");


	  for(int i=0;i<v->len; i++) {
	    if (d_equal((int)(v->v[i]), (v->v[i]))) {
	      printf(" %6d", (int)v->v[i]);
	    } else {
	      printf(" %6.3f", v->v[i]);
	    }
	    printf("\n");

	  }

	  printf("\n----------------\n");
	}


	double abs_val(double x){

	  if (x < (double)0.0){
	    return x*-1;
	  } else {
	    return x;
	  }
	}

	double pow_2(double b)
	{
    return (b*b);
	}


double sqrt_val(double n)
    {
        double x = n;

        double root;

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


	double dot(Matrix *A, Matrix *B){
	  double s = 0;
	  for (int i = 0; i <A->rows; i++){
	    for (int j=0; j<A->cols; j++){
	      s = s + (get(i,j,A) * get(i,j,B));
	    }
	  }
	  return s;
	}

	double vec_comb(Vector *a, Vector *b){
	  double s = 0;
	  for (int i = 0; i < a->len; i++){
	    s = s + (a->v[i] * b->v[i]);
	  }

	  return s;
	}

 Matrix *scal_div(Matrix *A, double s){
		Matrix *C = create_matrix(A->rows,A->cols,NULL);
	  for (int i = 0; i < A->rows*A->cols; i++){
	    C->m[i] = A->m[i] / s;
	  }
	  return C;
	}

	Matrix *mat_mul(Matrix *A, Matrix *B){
	  Matrix *C = create_matrix(A->rows,A->cols,NULL);
	   for (int i = 0; i < C->rows; ++i) {
	      for (int j = 0; j < C->cols; ++j) {
		 C->m[i*(C->cols)+j] = 0.0;
	      }
	   }

	   for (int v = 0; v < A->rows; ++v) {
	      for (int j = 0; j < B->cols; ++j) {
		 for (int k = 0; k < A->cols; ++k) {
		    C->m[v*(C->cols)+j] += get(v,k,A) * get(k,j,B);
		 }
	      }
	   }
	  return C;

	}

	//TODO CHECK
	Vector *vec_mul(Matrix *A, Vector *b){
	  Vector *c = create_vector(A->cols,NULL);

	   for (int i = 0; i < A->rows; ++i) {
			 for (int k = 0; k < A->cols; ++k) {
			    c->v[i] += get(i,k,A) * b->v[k];
			 }
	   }
	  return c;

	}


	double vec_vec_mul(Vector *a, Vector *b){
	  double c = 0.0;

	   for (int i = 0; i < a->len; ++i) {
	      c += a->v[i]*b->v[i];
	   }

	  return c;

	}

	Matrix *scal_mul(Matrix *A, double s){
		Matrix *C = create_matrix(A->rows,A->cols,NULL);
	  for (int i = 0; i < A->rows*A->cols; i++){
	    C->m[i] = A->m[i] * s;
	  }
	  return C;
	}

	Matrix *mat_add(Matrix *A, Matrix *B){
	  Matrix *C = create_matrix(A->rows,A->cols,NULL);
	  for (int i = 0; i < A->rows*A->cols; i++){
	    C->m[i] = A->m[i] + B->m[i];
	  }
	  return C;
	}

	Matrix *mat_sub(Matrix *A, Matrix *B){
	  Matrix *C = create_matrix(A->rows,A->cols,NULL);
	  for (int i = 0; i < A->rows*A->cols; i++){
	    C->m[i] = A->m[i] - B->m[i];
	  }
	  return C;
	}

	Matrix *mat_comb(Vector *y, Matrix_List *A){
	  Matrix *sum = scal_mul(A->m[0], y->v[0]);
    for (int i = 1; i < y->len; i++){
      sum = mat_add(sum,scal_mul(A->m[i], y->v[i]));
    }
	  return sum;
	}

	Matrix *transpose(Matrix *A){
	  Matrix *T = create_matrix(A->cols,A->rows,NULL);
	  for (int i = 0; i < T->rows; i++) {
	    for (int j = 0; j < T->cols; j++) {
		T->m[i*(T->cols)+j] = get(j,i,A);
	    }
	  }
	  return T;
	}

	Vector *vec_sub(Vector *a, Vector *b){
	  Vector *x = create_vector(a->len,NULL);
	  for (int i = 0; i < x->len; i++){
	    x->v[i] = a->v[i] - b->v[i];
	  }
	  return x;
	}


  Vector *vectorize(Matrix *A){
	  Vector *v = create_vector(A->rows*A->cols,NULL);
	  for (int i = 0; i<v->len; i++){
	    v->v[i] = A->m[i];
	  }
	  return v;
	}


	// matrix row manips
	void swap_rows(Matrix *X, int a, int b){
	  for (int j = 0; j < X->cols; j++){
	    double temp = get(a, j, X);
	    X->m[a*(X->cols)+j] = get(b, j, X);
	    X->m[b*(X->cols)+j] = temp;
	  }

	  //return X;
	}

	// divide row a in mat M by scalar s
	void div_row(Matrix *X, int a, double s){
	  for (int j = 0; j < X->cols; j++){
	    X->m[a*(X->cols)+j] = get(a, j, X) / s;
	  }

	  //return X;
	}

	Matrix *flatten(Matrix *X){
	  X->cols = X->cols*X->rows;
	  X->rows = 1;

	  return X;

	}

	// subtract (row b * scalar s) from row a
	void sub_multd_row(Matrix *X, int a, int b, double s){
	  for (int j = 0; j < X->cols; j++){
	    X->m[a*(X->cols)+j] = get(a, j, X) - (get(b, j, X)*s);
	  }

	  //return X;
	}

	double norm_circ(Vector *e, Matrix *LL, Vector *z){
	  //Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
	  Vector *res = create_vector(LL->cols,NULL);

	  for (int i = 0; i < LL->rows; ++i) {
	        for (int k = 0; k < LL->cols; ++k) {
	          res->v[i] += get(i,k,LL) * z->v[k];
	        }
	  }
		for (int j = 0; j < res->len; j++){
			res->v[j] = e->v[j] - res->v[j];
		}


	  double err = norm_vec_big(res);
	  return err;

	}

	double norm_vec_big(Vector *v){

	  double sum = 0.0;
	  for (int i=0; i<v->len; i++){
	    sum = sum + pow_2(v->v[i]);
	  }

	  double r = sqrt(sum);
	  return r;
	}

	double norm_mat_circ(Matrix *L, Matrix *D){
  //Matrix I = mat_mul(mat_mul(inverse(L),D),inverse(transpose(L)));

  //LI = inverse(L)
  //Matrix_List LU_List = LUP_decompose(L);
  Matrix *LA = create_matrix(L->rows,L->cols,NULL);
  Matrix *UA = create_matrix(L->rows,L->cols,NULL);
  Matrix *PA = create_matrix(L->rows,L->cols,NULL);
  int n = L->rows;

  //pivot A P
  for (int ld=0; ld<n;ld++){
    for (int gd=0; gd<n; gd++){
      if (ld == gd){
        PA->m[ld*(LA->cols)+gd] = 1.0;
      }
    }
  }

  for (int yd=0; yd<n; yd++){
    int max_j = yd;
    for (int rd = yd; rd < n; rd++){
      if (abs_val(get(rd,yd,L)) > abs_val(get(max_j,yd,L))){
        max_j = rd;
      }
    }

    if (max_j != yd){
      //PD = swap_rows_big(PD,yd,max_j);
      for (int sr = 0; sr < PA->cols; sr++){
        double temp = get(yd, sr, PA);
        PA->m[yd*(PA->cols)+sr] = get(max_j, sr, PA);
        PA->m[max_j*(PA->cols)+sr] = temp;
      }
    }
  }

  Matrix *LLp = mat_mul(PA,L);
/*  Matrix *LLp = create_matrix(L->rows,NULL);

  for (int vll = 0; vll < PA->rows; ++vll) {
      for (int jll = 0; jll < L->cols; ++jll) {
        for (int kll = 0; kll < PA->cols; ++kll) {
            LLp->m[vll*(LLp->cols)+jll] += get(vll,kll,PA) * get(kll,jll,L);
        }
      }
  }
	*/

  for (int vd=0; vd<n; vd++){
    LA->m[vd*(LA->cols)+vd] = 1.0;
  }

  for (int ig=0;ig<n;ig++){

    for (int je=0;je<n;je++){
      double se;
      if (je <= ig){
        se = 0;
        for (int ce = 0; ce < je; ce++){
          se += get(je,ce,LA) * get(ce,ig,UA);
        }
        UA->m[je*(UA->cols)+ig] = (get(je,ig,LLp) - se);
      }
      if (je >= ig){
        se = 0;
        for (int ke = 0; ke < ig; ke++){
          se += get(je,ke,LA) * get(ke,ig,UA);
        }
        LA->m[je*(L->cols)+ig] = (get(je,ig,LLp) - se) / get(ig,ig,UA);
      }
    }
  }


  Matrix *IDT = create_matrix(L->rows,L->cols,NULL);
  for (int t = 0; t < IDT->rows; t++){
    for (int u = 0; u < IDT->cols; u++){
      if (t == u){
        IDT->m[t*(IDT->cols)+u] = 1.0;
      }
    }
  }

  //Big_Matrix_List LU_List = {2, , PD};

  //Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
  Matrix *LU = create_matrix(LA->rows,LA->cols,NULL);
  for (int tu = 0; tu < LU->rows; tu++){
    for (int uu = 0; uu < LU->cols; uu++){
      LU->m[tu*(LU->cols)+uu] = get(tu,uu,LA) + get(tu,uu,UA) - get(tu,uu,IDT);
    }
  }

  // end decomp

  Matrix *P = PA;
  Matrix *LI = LU;

  for (int i = 0; i<n; i++){
    Vector *bb = create_vector(n,NULL);
    for (int j = 0; j<n; j++){
      if (i==j){
        bb->v[j] = 1.0;
      }
    }

    //b = vec_mul_big(biggify_mat(P,P->rows,P->cols),b);
    Vector *b = create_vector(P->cols,NULL);

    for (int iq = 0; iq < P->rows; ++iq) {
          for (int kq = 0; kq < P->cols; ++kq) {
              b->v[iq] += get(iq,kq,P) * bb->v[kq];
          }
    }

      // forward substitute
    for (int d = 0; d < n-1; d++){
      for (int e = d+1; e < n; e++){
        b->v[e] = b->v[e] - (get(e,d,LU) * b->v[d]);
      }
    }

    // backwards substitute
    for (int f = (n-1); f >= 0; f--){
      if (abs_val(get(f,f,LU)) < epsilon){
        valid = 0;
      }
      b->v[f] = b->v[f] / get(f,f,LU);
      for (int h = 0; h < f; h++){
        b->v[h] = b->v[h] - (get(h,f,LU) * b->v[f]);
      }
    }

    for (int k = 0; k < n; k++){
        LI->m[k*(LI->cols)+i] = b->v[k];//TODO
    }
  }

  //LT = transpose(L)
  Matrix *LT = L;
  for (int itt = 0; itt < LT->rows; itt++) {
    for (int jtt = 0; jtt < LT->cols; jtt++) {
        LT->m[itt*(LT->cols)+jtt] = get(jtt,itt,L);
    }
  }

  //LTI = inverse(L->T)
 //Matrix_List LU_List = LUP_decompose(LT);
  Matrix *LB = create_matrix(L->rows,L->cols,NULL);
  Matrix *UB = create_matrix(L->rows,L->cols,NULL);
  Matrix *PB = create_matrix(L->rows,L->cols,NULL);

  //pivot A P
  for (int ld2=0; ld2<n;ld2++){
    for (int gd2=0; gd2<n; gd2++){
      if (ld2 == gd2){
        PB->m[ld2*(PB->cols)+gd2] = 1.0;
      }
    }
  }

  for (int yd2=0; yd2<n; yd2++){
    int max_j2 = yd2;
    for (int rd2 = yd2; rd2 < n; rd2++){
      if (abs_val(get(rd2,yd2,LT)) > abs_val(get(max_j2,yd2,LT))){
        max_j2 = rd2;
      }
    }

    if (max_j2 != yd2){
      //PD = swap_rows_big(PD,yd,max_j);
      for (int sr2 = 0; sr2 < PB->cols; sr2++){
        double temp = get(yd2, sr2, PB);
        PB->m[yd2*(PB->cols)+sr2] = get(max_j2, sr2, PB);
        PB->m[max_j2*(PB->cols)+sr2] = temp;
      }
    }
  }

  //Big_Matrix LLp = mat_mul_big(PD,LL);
  Matrix *LLpp = create_matrix(L->rows,L->cols,NULL);

  for (int vll2 = 0; vll2 < L->rows; ++vll2) {
      for (int jll2 = 0; jll2 < L->rows; ++jll2) {
        for (int kll2 = 0; kll2 < L->rows; ++kll2) {
            LLpp->m[vll2*(LLpp->cols)+jll2] += get(vll2,kll2,PB) * get(kll2,jll2,LT);
        }
      }
  }

  for (int vd2=0; vd2<n; vd2++){
    LB->m[vd2*(LB->cols)+vd2] = 1.0;
  }

  for (int ig2=0;ig2<n;ig2++){

    for (int je2=0;je2<n;je2++){
      double se2;
      if (je2 <= ig2){
        se2 = 0;
        for (int ce2 = 0; ce2 < je2; ce2++){
          se2 += get(je2,ce2,LB) * get(ce2,ig2,UB);
        }
        UB->m[je2*(UB->cols)+ig2] = (get(je2,ig2,LLpp) - se2);
      }
      if (je2 >= ig2){
        se2 = 0;
        for (int ke2 = 0; ke2 < ig2; ke2++){
          se2 += get(je2,ke2,LB) * get(ke2,ig2,UB);
        }
        LB->m[je2*(LT->cols)+ig2] = (get(je2,ig2,LLpp) - se2) / get(ig2,ig2,UB);
      }
    }
  }


  //Big_Matrix_List LU_List = {2, , PD};

Matrix *LU2 = mat_sub(mat_add(LB,UB),IDT);
/*  Matrix LU2 = create_matrix(LB->rows,LB->cols,NULL);
  for (int tu2 = 0; tu2 < N; tu2++){
    for (int uu2 = 0; uu2 < N; uu2++){
      LU2->m[tu2*(LU2->cols)+uu2] = get(tu2,uu2,LB) + get(tu2,uu2,UB) - get(tu2,uu2,IDT);
    }
  }
*/

  // end decomp

  Matrix *LTI = LU2;

  for (int i2 = 0; i2<n; i2++){
    Vector *bb2 = create_vector(n,NULL);
    for (int j2 = 0; j2<n; j2++){
      if (i2==j2){
        bb2->v[j2] = 1.0;
      }
    }

    //b = vec_mul_big(biggify_mat(P,P->rows,P->cols),b);
    Vector *b2 = create_vector(PB->cols,NULL);

    for (int i3 = 0; i3 < PB->rows; ++i3) {
          for (int k3 = 0; k3 < PB->cols; ++k3) {
              b2->v[i3] += get(i3,k3,PB) * bb2->v[k3];
          }
    }

      // forward substitute
    for (int d3 = 0; d3 < n-1; d3++){
      for (int e3 = d3+1; e3 < n; e3++){
        b2->v[e3] = b2->v[e3] - (get(e3,d3,LU2) * b2->v[d3]);
      }
    }

    // backwards substitute
    for (int f3 = (n-1); f3 >= 0; f3--){
      if (abs_val(get(f3,f3,LU2)) < epsilon){
        valid = 0;
      }
      b2->v[f3] = b2->v[f3] / get(f3,f3,LU2);
      for (int h3 = 0; h3 < f3; h3++){
        b2->v[h3] = b2->v[h3] - (get(h3,f3,LU2) * b2->v[f3]);
      }
    }

    for (int k4 = 0; k4 < n; k4++){
        LTI->m[k4*(LTI->cols)+i2] = b2->v[k4];//TODO
    }
  }
  // end inverse

  //I = mul: (L->I * D) * L->T->I
  Matrix *LID = create_matrix(L->rows,L->cols,NULL);
  for (int v = 0; v < L->rows; ++v) {
    for (int jd = 0; jd < L->rows; ++jd) {
        for (int k = 0; k < L->rows; ++k) {
          LID->m[v*(LID->cols)+jd] += get(v,k,LI) * get(k,jd,D);
        }
    }
  }
  Matrix *I = create_matrix(L->rows,L->cols,NULL);
  for (int vi = 0; vi < L->rows; ++vi) {
    for (int ji = 0; ji < L->rows; ++ji) {
        for (int ki = 0; ki < L->rows; ++ki) {
          I->m[vi*(I->cols)+ji] += get(vi,ki,LID) * get(ki,ji,LTI);
        }
    }
  }

  //printf("I EQ");
  //print_mat(I);

  double stop = norm_mat(I);

  return stop;
}


	double norm_mat(Matrix *X){
	  double sum = 0.0;
	  for (int i=0; i<X->rows; i++){
	    for (int j=0; j<X->cols; j++){
	      sum = sum + pow_2(get(i,j,X));
	    }
	  }

	  double r = sqrt(sum);
	  return r;
	}

/*
	Matrix_List LUP_decompose_big(Big_Matrix A){

	  Big_Matrix L = {A->rows,A->cols,{0}};
	  Big_Matrix U = {A->rows,A->cols,{0}};
	  Big_Matrix P = {A->rows,A->cols,{0}};
	  int n = A->rows;

	  //pivot A P
	  for (int l=0; l<n;l++){
	    for (int g=0; g<n; g++){
	      P->m[l*(L->cols)+g] = (l == g);
	    }
	  }

	  for (int y=0; y<n; y++){
	    int max_j = y;
	    for (int r = y; r < n; r++){
	      if (abs_val(get(r,y,A)) > abs_val(get(max_j,y,A))){
		max_j = r;
	      }
	    }

	    if (max_j != y){
	      P = swap_rows_big(P,y,max_j);
	    }
	  }

	  Big_Matrix Ap = mat_mul_big(P,A);
    //printf("REAL AP");
    //print_mat_b(Ap);

	  for (int v=0; v<n; v++){
	    L->m[v*(L->cols)+v] = 1.0;
	  }

	  for (int i=0;i<n;i++){

	    for (int j=0;j<n;j++){
	      double s;
	      if (j <= i){
          s = 0;
          for (int c = 0; c < j; c++){
            s += get(j,c,L) * get(c,i,U);
          }
          U->m[j*(U->cols)+i] = (get(j,i,Ap) - s);
        }
        if (j >= i){
          s = 0;
          for (int k = 0; k < i; k++){
            s += get(j,k,L) * get(k,i,U);
          }
          L->m[j*(L->cols)+i] = (get(j,i,Ap) - s) / get(i,i,U);
	      }
	    }
	  }

    //printf("REAL B4I");
    //print_mat_b(L);
    //print_mat_b(U);

	  Big_Matrix I = {A->rows,A->cols,{0}};

	  for (int t = 0; t < I->rows; t++){
	    for (int u = 0; u < I->cols; u++){
	      if (t == u){
		I->m[t*(I->cols)+u] = 1.0;
	      }
	    }
	  }

	  Big_Matrix_List LU_List = {2, {mat_sub_big(mat_add_big(L,U),I), P}};

	  return LU_List;


	} */
/*
	Matrix_List LUP_decompose(Matrix A){

	  Matrix L = {A->rows,A->cols,{0}};
	  Matrix U = {A->rows,A->cols,{0}};
	  Matrix P = {A->rows,A->cols,{0}};
	  int n = A->rows;

	  //pivot A P
	  for (int l=0; l<n;l++){
	    for (int g=0; g<n; g++){
	      P->m[l*(L->cols)+g] = (l == g);
	    }
	  }

	  for (int y=0; y<n; y++){
	    int max_j = y;
	    for (int r = y; r < n; r++){
	      if (abs_val(get(r,y,A)) > abs_val(get(max_j,y,A))){
		max_j = r;
	      }
	    }

	    if (max_j != y){
	      swap_rows(P,y,max_j);
	    }
	  }

	  Matrix Ap = mat_mul(P,A);

	  for (int v=0; v<n; v++){
	    L->m[v*(L->cols)+v] = 1.0;
	  }

	  for (int i=0;i<n;i++){

	    for (int j=0;j<n;j++){
	      double s;
	      if (j <= i){
		s = 0;
		for (int c = 0; c < j; c++){
		  s += get(j,c,L) * get(c,i,U);
		}
		U->m[j*(U->cols)+i] = (get(j,i,Ap) - s);
	      }
	      if (j >= i){
		s = 0;
		for (int k = 0; k < i; k++){
		  s += get(j,k,L) * get(k,i,U);
		}
		L->m[j*(L->cols)+i] = (get(j,i,Ap) - s) / get(i,i,U);
	      }
	    }
	  }

	  Matrix I = {A->rows,A->cols,{0}};

	  for (int t = 0; t < I->rows; t++){
	    for (int u = 0; u < I->cols; u++){
	      if (t == u){
		I->m[t*(I->cols)+u] = 1.0;
	      }
	    }
	  }

	  Matrix_List LU_List = {2, {mat_sub(mat_add(L,U),I), P}};

	  return LU_List;


	}
*/

/*
	Big_Vector LUP_solve(Big_Matrix A, Big_Vector b){

	  Big_Matrix_List LU_List = LUP_decompose_big(A);

	  Big_Matrix LU = LU_List->m[0];
	  Big_Matrix P = LU_List->m[1];

	  int n = LU->rows;

	  Big_Vector f = vec_mul_big(P,b);

	  // forward substitute
	  for (int a = 0; a < n-1; a++){
	    for (int b = a+1; b < n; b++){
	      f->v[b] = f->v[b] - (get(b,a,LU) * f->v[a]);
	    }
	  }

	  // backwards substitute
	  for (int i = (n-1); i >= 0; i--){
	    if (abs_val(get(i,i,LU)) < epsilon){
	      valid = 0;
	    }
	    f->v[i] = f->v[i] / get(i,i,LU);
	    for (int j = 0; j < i; j++){
	      f->v[j] = f->v[j] - (get(j,i,LU) * f->v[i]);
	    }
	  }

	  return f; //TODO CHANGE

	}
	*/


	/*Eq_Sol solve_eq(Matrix X, Matrix_List A, Matrix C, double theta){

	  Matrix D = {N,N,{0}};
	  Vector y = {M, {0}};

	  //set up
	  Matrix U = mat_sub(X,scal_div(mat_mul(mat_mul(X,C),X),theta));

	  Big_Matrix K = biggify_mat(flatten(U),U->rows,U->cols);
	  int total = U->rows*U->cols;
	  for (int i=total; i < total+M; i++){
	    K->m[i] = 0.0;
	  }
	  K->cols += M;


	  Big_Matrix Q = {N*N,M,{0}};
	  Matrix nX = scal_mul(X,-1);

	  Matrix_List AQ = A;

	  //for(int a = 0; a < A->len; a++){
	  AQ->m0 = scal_div(mat_mul(mat_mul(nX,AQ->m0),X),theta);
	  AQ->m0 = flatten(AQ->m0);

	  AQ->m1 = scal_div(mat_mul(mat_mul(nX,AQ->m1),X),theta);
	  AQ->m1 = flatten(AQ->m1);

	  //}
	  for (int c = 0; c < Q->rows; c++){
	    //for (int b = 0; b < Q->cols; b++){
	    Q->m[c*(Q->cols)+0] = get(0,c,AQ->m0);
	    Q->m[c*(Q->cols)+1] = get(0,c,AQ->m1);
	    //}
	  }


	  Big_Matrix R = {M,N*N,{0}};
	  //for (int d = 0; d < R->rows; d++){
	  for (int e = 0; e < R->cols; e++){
	    R->m[0*(R->cols)+e] = A->m0->m[e];
	  }
	  for (int e = 0; e < R->cols; e++){
	    R->m[1*(R->cols)+e] = A->m1->m[e];
	  }


	  Big_Matrix L = {(Q->rows + R->rows),(N*N + Q->cols),{0}};
	  //prepend identity matrix to Q
	  for (int f = 0; f < Q->rows; f++){
	    for (int g = 0; g < N*N; g++){
	      L->m[f*(L->cols)+g] = (f == g);
	    }
	    for (int g = N*N; g < L->cols; g++){
	      L->m[f*(L->cols)+g] = get(f,(g-N*N),Q);
	    }
	  }
	  // append P, zeros
	  for (int i = Q->rows; i < L->rows; i++){
	    for (int j = 0; j < R->cols; j++){
	      L->m[i*(L->cols)+j] = get((i-Q->rows),j,R);
	    }
	    for (int h = R->cols; h < L->cols; h++){
	      L->m[i*(L->cols)+h] = 0.0;
	    }
	  }

	  //least sq solution
	  Big_Vector e = vectorize(K);

	  Big_Vector z = LUP_solve(L, e);


	  //norm - for res error;
	  Big_Vector res = vec_sub_big(e,vec_mul_big(L,z));
	  double err = norm_vec_big(res);

	  //clean up D, y

	  for (int l = 0; l < N*N; l++){
	    D->m[l] = z->v[l];
	  }

	  D = scal_mul(mat_add(D,transpose(D)),0.5);

	  for (int o = N*N; o < (N*N)+M; o++){
	    y->v[o-(N*N)] = z->v[o];
	  }



	  return sol;
	}*/


	Matrix *cholesky(Matrix *X){
	  Matrix *R = create_matrix(X->rows,X->cols,NULL);

	  for (int i = 0; i < X->rows; i++) {
	    for (int j = 0; j < (i+1); j++) {
	      double s = 0.0;

	      for (int k = 0; k < j; k++) {
		s = s + R->m[i*(R->cols)+k] * R->m[j*(R->cols)+k];
	      }

	      if (i == j){
		double to_root = get(i,i,X) - s;
		R->m[i*(R->cols)+j] = sqrt(to_root);
	      } else {
		R->m[i*(R->cols)+j] = (double)1.0 / get(j,j,R) * (get(i,j,X) - s);
	      }
	    }
	  }
	  return R;
	}


  int psd(Matrix *X){ // X is psd <-> Cholesky decomposable

  int r = 1;

  int n = X->rows;
  Matrix *R = create_matrix(X->rows,X->cols,NULL);

  for (int i = 0; i < n; i++) {

    for (int j = 0; j < (i+1); j++) {
      double s = 0.0;
      for (int k = 0; k < j; k++) {
        s = s + R->m[i*(n)+k] * R->m[j*(n)+k];
      }

      if (i == j){

        double to_root = X->m[i*n+i] - s;
        if (to_root < (double)(0.0)){
          r = 0;
        }
        R->m[i*(n)+j] = sqrt(to_root);
      } else {
        R->m[i*(n)+j] = (double)(1.0) / R->m[j*n+j] * (X->m[i*n+j] - s);
      }
    }
  }
	free_matrix(R);

  //print_mat(R);
  return r;
}
/*
	Matrix inverse(Matrix X){

	  Matrix_List LU_List = LUP_decompose(X);

	  Matrix LU = LU_List->m[0];

	  Matrix P = LU_List->m[1];
	  Matrix I = LU;
	  int n = LU->rows;

	  for (int i = 0; i<n; i++){
	    Big_Vector b = {n, {0}};
	    for (int j = 0; j<n; j++){
	      if (i==j){
		b->v[j] = 1.0;
	      } else {
		b->v[j] = 0.0;
	      }
	    }

	    b = vec_mul_big(biggify_mat(P,P->rows,P->cols),b);

	      // forward substitute
	    for (int d = 0; d < n-1; d++){
	      for (int e = d+1; e < n; e++){
		b->v[e] = b->v[e] - (get(e,d,LU) * b->v[d]);
	      }
	    }

	    // backwards substitute
	    for (int f = (n-1); f >= 0; f--){
	      if (abs_val(get(f,f,LU)) < epsilon){
		valid = 0;
	      }
	      b->v[f] = b->v[f] / get(f,f,LU);
	      for (int h = 0; h < f; h++){
		b->v[h] = b->v[h] - (get(h,f,LU) * b->v[f]);
	      }
	    }

	    for (int k = 0; k < n; k++){
		I->m[k*(I->cols)+i] = b->v[k];//TODO
	    }
	  }
	  return I;
	}

*/
/*
	Matrix *matrixize(Vector *a, int rows, int cols){

	  Matrix X;
	  X->rows = rows;
	  X->cols = cols;

	  for (int i = 0; i < a->len; i++){
	    X->m[i] = a->v[i];

	  }

	  return X;

	}
*/

	/*Matrix make_X0(Problem P){

	  Matrix H;
	  H->rows = P->A->len;
	  H->cols = P->A->mats[0]->rows * (P->A->mats[0]->rows+1) /2;

	  for (int i = 0; i<P->A->len; i++){
	    Vector ai = sym_vectorize(P->A->mats[i]);

	    for (int j=0; j < H->cols; j++){
	      H->m[i*(H->cols)+j] = ai->v[j];
	    }
	  }

	  Gauss_Vec x = gauss_solve(H, P->b);

	  if (x->sols == 1){
	    return matrixize(reg_vec(x), P->X->rows, P->X->cols);
	  } else if (x->sols == -1){

	  } else { //inifite TODO

	  }



	  return  matrixize(reg_vec(x), P->X->rows, P->X->cols);

	} */

	Solution *sdp(int N, int M, Matrix *C, Matrix *Xp, Matrix_List *A, Vector *b, double theta, double beta){


	  // Reference: http://ocw->mit->edu/courses/electrical-engineering-and-computer-science/6-251j-introduction-to-mathematical-programming-fall-2009/readings/MIT6_251JF09_SDP->pdf
	  int p = psd(Xp);

		Solution *Q = (Solution *)malloc(sizeof(Solution));
		Q->feasible = 0;
		Q->X = Xp;
		Q->y = create_vector(M, NULL);

		Vector *new_y = create_vector(M, NULL);

    int eq = 1;
    for (int i = 0; i < M; i++){
      eq = eq && (d_equal(dot(A->m[i], Xp),b->v[i]));
    }

	  if (p && eq){

	    //beta approx
	    //Q->X = beta_approx(C,Q->X,A,b,theta,beta);

	    double stop = 1.0;
	    double err = 0.0;

      int loop = 0;
	    while(!(err > (double)1e-2) && !(stop <= (double)0.25)){
        loop++;
        //printf("beta X");
        //print_mat(Q->X);

	      Matrix *L = create_matrix(N,N, NULL); //cholesky(Q->X);

	      for (int ii = 0; ii < N; ii++) {
					for (int jj = 0; jj < (ii+1); jj++) {
					  double s = 0.0;

					  for (int kk = 0; kk < jj; kk++) {
					    s = s + L->m[ii*(L->cols)+kk] * L->m[jj*(L->cols)+kk];
					  }

					  if (ii == jj){
					    double to_root = get(ii,ii,Q->X) - s;
					    L->m[ii*(L->cols)+jj] = sqrt(to_root);
					  } else {
					    L->m[ii*(L->cols)+jj] = (double)1.0 / get(jj,jj,L) * (get(ii,jj,Q->X) - s);
					  }
					}
	      }
	      //end cholesky

        //printf("L");
        //print_mat(L);

	      //Eq_Sol sol = solve_eq(Q->X,A,C,theta);

	      Matrix *D = create_matrix(N,N, NULL);

	      //set up
	      //Matrix U = mat_sub(Q->X,scal_div(mat_mul(mat_mul(Q->X,C),Q->X),theta));
	      Matrix *U1 = create_matrix(N,N, NULL);
	      for (int v = 0; v < U1->rows; ++v) {
				  for (int j = 0; j < U1->cols; ++j) {
				    for (int k = 0; k < U1->cols; ++k) {
							U1->m[v*(U1->cols)+j] += get(v,k,Q->X) * get(k,j,C);
				    }
				  }
	      }

	      Matrix *U = create_matrix(N,N, NULL);
	      for (int vv = 0; vv < U->rows; ++vv) {
				  for (int j = 0; j < U->cols; ++j) {
				    for (int k = 0; k < U->cols; ++k) {
							U->m[vv*(U->cols)+j] += get(vv,k,U1) * get(k,j,Q->X);
				    }
				  }
	      }

	      for (int ie = 0; ie < U->rows*U->cols; ie++){
					U->m[ie] = Q->X->m[ie] - (U->m[ie] / theta);
	      }

	     double K[500] = {0};
	     int Krows = 1;
	     int Kcols = U->cols*U->rows;

	     for (int id=0; id<U->rows*U->cols; id++){
					K[id] = U->m[id];
	      }
	      Kcols += M;




	      Matrix *QQ = create_matrix(N*N,M, NULL);
	      Matrix *nX = create_matrix(N,N, NULL); //scal_mul(Q->X,-1);
	      for (int ib = 0; ib < Q->X->rows*Q->X->cols; ib++){
					nX->m[ib] = Q->X->m[ib] * -1;
	      }

        //printf("nX");
        //print_mat(nX);

	      //double AQm0[9] = {0};
	      //double AQm1[9] = {0};
	      int AQrows = N;
	      int AQcols = N;


	      //for(int a = 0; a < A->len; a++){
	      //AQ->m0 = scal_div(mat_mul(mat_mul(nX,AQ->m0),Q->X),theta);
	      Matrix_List *AT = create_matrix_list(M, N, NULL);

	      for (int vb = 0; vb < N; ++vb) {
				  for (int jb = 0; jb < N; ++jb) {
				    for (int kb = 0; kb < N; ++kb) {
              for (int i = 0; i < M; i++){
                AT->m[i]->m[vb*(AQcols)+jb] += get(vb,kb,nX) * get(kb,jb,A->m[i]);
              }
							//AT0->m[vb*(AQcols)+jb] += get(vb,kb,nX) * get(kb,jb,A->m0);
							//AT1->m[vb*(AQcols)+jb] += get(vb,kb,nX) * get(kb,jb,A->m1);
				    }
				  }
	      }

        Matrix_List *ATT = create_matrix_list(M, N, NULL);

	      for (int va = 0; va < N; ++va) {
				  for (int ja = 0; ja < N; ++ja) {
				    for (int ka = 0; ka < N; ++ka) {
              for (int i = 0; i < M; i++){
                ATT->m[i]->m[va*(AQcols)+ja] += get(va,ka,AT->m[i]) * get(ka,ja,Q->X);
              }
							//AT00->m[va*(AQcols)+ja] += get(va,ka,AT0) * get(ka,ja,Q->X);
							//AT11->m[va*(AQcols)+ja] += get(va,ka,AT1) * get(ka,ja,Q->X);
				    }
				  }
	      }

	      for (int ia = 0; ia < AQrows*AQcols; ia++){
          for (int i = 0; i < M; i++){
            ATT->m[i]->m[ia] = ATT->m[i]->m[ia] / theta;
          }
	      }
	      //AQ->m0 = flatten(AQ->m0);
	      AQcols = AQcols*AQrows;
	      AQrows = 1;

	      //AQ->m1 = scal_div(mat_mul(mat_mul(nX,AQ->m1),Q->X),theta);
	      //AQ->m1 = flatten(AQ->m1);

	      //}
	      for (int c = 0; c < QQ->rows; c++){
          for (int i = 0; i < M; i++){
            QQ->m[c*(QQ->cols)+i] = get(0,c,ATT->m[i]);
          }
					//for (int b = 0; b < Q->cols; b++){
					//QQ->m[c*(QQ->cols)+0] = get(0,c,AT00);
					//QQ->m[c*(QQ->cols)+1] = get(0,c,AT11);
					//}
	      }


	      Matrix *R = create_matrix(M,N*N, NULL);
	      //for (int d = 0; d < R->rows; d++){
	      for (int ee = 0; ee < R->cols; ee++){
          for (int i = 0; i < M; i++){
            R->m[i*(R->cols)+ee] = A->m[i]->m[ee];
          }
					//R->m[0*(R->cols)+ee] = A->m0->m[ee];
					//R->m[1*(R->cols)+ee] = A->m1->m[ee];
	      }

        //printf("R");
        //print_mat_b(R);

	      Matrix *LL = create_matrix((QQ->rows + R->rows),(N*N + QQ->cols), NULL);
          //prepend identity matrix to Q
          for (int fq = 0; fq < QQ->rows; fq++){
            for (int gq = 0; gq < N*N; gq++){
              if (fq == gq){
                LL->m[fq*(LL->cols)+gq] = 1.0;
              }
          }
          for (int gg = N*N; gg < LL->cols; gg++){
            LL->m[fq*(LL->cols)+gg] = get(fq,(gg-N*N),QQ);
          }
        }
        // append P, zeros
        for (int ip = QQ->rows; ip < LL->rows; ip++){
          for (int jp = 0; jp < R->cols; jp++){
            LL->m[ip*(LL->cols)+jp] = get((ip-QQ->rows),jp,R);
          }
          for (int hp = R->cols; hp < LL->cols; hp++){
            LL->m[ip*(LL->cols)+hp] = 0.0;
          }
        }


        //least sq solution
        //Big_Vector e = vectorize(K);
        Vector *e = create_vector(Krows*Kcols, NULL);
        for (int iik = 0; iik<e->len; iik++){
          e->v[iik] = K[iik];
        }

        //Big_Vector z = LUP_solve(LL, e);
         //Big_Matrix_List LU_List = LUP_decompose_big(LL);
         ////printf("REAL LU");
         ////print_mat_b(LU_List->m0);


        Matrix *LD = create_matrix(LL->rows,LL->cols, NULL);
        Matrix *UD = create_matrix(LL->rows,LL->cols, NULL);
        Matrix *PD = create_matrix(LL->rows,LL->cols, NULL);
        int n = LL->rows;

        //pivot A P
        for (int ld=0; ld<n;ld++){
          for (int gd=0; gd<n; gd++){
            if (ld == gd){
              PD->m[ld*(LD->cols)+gd] = 1.0;
            }
          }
        }

        for (int yd=0; yd<n; yd++){
          int max_j = yd;
          for (int rd = yd; rd < n; rd++){
            if (abs_val(get(rd,yd,LL)) > abs_val(get(max_j,yd,LL))){
              max_j = rd;
            }
          }

          if (max_j != yd){
            //PD = swap_rows_big(PD,yd,max_j);
            for (int sr = 0; sr < PD->cols; sr++){
              double temp = get(yd, sr, PD);
              PD->m[yd*(PD->cols)+sr] = get(max_j, sr, PD);
              PD->m[max_j*(PD->cols)+sr] = temp;
            }
          }
        }



        //Big_Matrix LLp = mat_mul_big(PD,LL);
        Matrix *LLp = create_matrix(PD->rows,LL->cols, NULL);

        for (int vll = 0; vll < PD->rows; ++vll) {
            for (int jll = 0; jll < LL->cols; ++jll) {
              for (int kll = 0; kll < PD->cols; ++kll) {
                  LLp->m[vll*(LLp->cols)+jll] += get(vll,kll,PD) * get(kll,jll,LL);
              }
            }
        }

        //printf("MY AP");
        //print_mat_b(LLp);

        for (int vd=0; vd<n; vd++){
          LD->m[vd*(LD->cols)+vd] = 1.0;
        }

        //printf("MY PRE L");
        //print_mat_b(LD);



        for (int ig=0;ig<n;ig++){

          for (int je=0;je<n;je++){
            double se;
            if (je <= ig){
              se = 0;
              for (int ce = 0; ce < je; ce++){
                se += get(je,ce,LD) * get(ce,ig,UD);
              }
              UD->m[je*(UD->cols)+ig] = (get(je,ig,LLp) - se);
            }
            if (je >= ig){
              se = 0;
              for (int ke = 0; ke < ig; ke++){
                se += get(je,ke,LD) * get(ke,ig,UD);
              }
              LD->m[je*(LD->cols)+ig] = (get(je,ig,LLp) - se) / get(ig,ig,UD);
            }
          }
        }


        Matrix *I = create_matrix(LL->rows,LL->cols, NULL);

        for (int t = 0; t < I->rows; t++){
          for (int u = 0; u < I->cols; u++){
            if (t == u){
              I->m[t*(I->cols)+u] = 1.0;
            }
          }
        }

        //Big_Matrix_List LU_List = {2, , PD};


        // end decomp

        //Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
        Matrix *LU = create_matrix(LD->rows,LD->cols, NULL);
        for (int tu = 0; tu < I->rows; tu++){
          for (int uu = 0; uu < I->cols; uu++){
            LU->m[tu*(LU->cols)+uu] = get(tu,uu,LD) + get(tu,uu,UD) - get(tu,uu,I);
          }
          }



          //Big_Vector z = vec_mul_big(PD,e);
        Vector *z = create_vector(PD->cols, NULL);

        for (int iz = 0; iz < PD->rows; ++iz) {
          for (int kz = 0; kz < PD->cols; ++kz) {
              z->v[iz] += get(iz,kz,PD) * e->v[kz];
          }
        }

        // forward substitute
        for (int a = 0; a < n-1; a++){
          for (int b = a+1; b < n; b++){
            z->v[b] = z->v[b] - (get(b,a,LU) * z->v[a]);
          }
        }

        // backwards substitute
        for (int i = (n-1); i >= 0; i--){
          if (abs_val(get(i,i,LU)) < epsilon){
            valid = 0;
          }
          z->v[i] = z->v[i] / get(i,i,LU);
          for (int j = 0; j < i; j++){
            z->v[j] = z->v[j] - (get(j,i,LU) * z->v[i]);
          }
        }

        //end LUP solve

        //printf("Z");
        //print_vec_b(z);

        //norm - for res error;
        //Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
        //double err = norm_vec_big(res);
        double err = norm_circ(e, LL, z);

        for (int l = 0; l < N*N; l++){
          D->m[l] = z->v[l];
        }

        //D = scal_mul(mat_add(D,transpose(D)),0.5);
        Matrix *T = D;
        for (int it = 0; it < T->rows; it++) {
          for (int jt = 0; jt < T->cols; jt++) {
              T->m[it*(T->cols)+jt] = get(jt,it,D);
          }
        }
        for (int kt = 0; kt < D->rows*D->cols; kt++){
          D->m[kt] = (D->m[kt] + T->m[kt]) * 0.5;
        }


        for (int o = N*N; o < (N*N)+M; o++){
          new_y->v[o-(N*N)] = z->v[o];
        }



        // end solve_eq
        //printf("Y");
        //print_vec(new_y);

        //printf("D");
        //print_mat(D);

        //if (err <= 1e-2){

          Q->y = new_y;

          //Matrix S = mat_sub(C, mat_comb(y,A));

          //Matrix I = mat_mul(mat_mul(inverse(L),D),inverse(transpose(L)));
          //stop = norm_mat(I);
          stop = norm_mat_circ(L, D);

          //printf("STOP %f\n", stop);

          if (stop > 0.25){
            double alpha = 0.2 / stop;

            Matrix *XaD = create_matrix(N,N, NULL);//mat_add(Q->X,scal_mul(D,alpha));
            for (int xx = 0; xx < D->rows*D->cols; xx++){
              XaD->m[xx] = Q->X->m[xx] + (D->m[xx] * alpha);
            }
            while (!psd(XaD)){
              alpha = alpha * 0.5;


              //printf("QX TRANS");
              //print_mat(Q->X);
              for (int xx = 0; xx < D->rows*D->cols; xx++){
                XaD->m[xx] = Q->X->m[xx] + (D->m[xx] * alpha);
              }

            }

            //Q->X = mat_add(Q->X,scal_mul(D,alpha));
            for (int xx = 0; xx < D->rows*D->cols; xx++){
              Q->X->m[xx] = Q->X->m[xx] + (D->m[xx] * alpha);
            }
            /*
            //printf("QX TRANS");
            //print_mat(Q->X);
            */

          //}
        }



	    } // end while


			//solving - real SDP
	    double errs = 0.0;
      loop = 0;
	    while(!(errs > 1e-2) && !(theta < 1e-4)){
        loop++;
        //printf("theta = %f, loop = %d\n", theta, loop);
        //printf("reg X");
        //print_mat(Q->X);

				//2-> shrink T (Theta);
	      double alpha = 0.8; //1 - ((sqrt(beta) - beta)/(sqrt(b)+sqrt(n))); //alpha in (0, 1)

	      theta = alpha*theta;

	      //3-> compute newton direction and multipliers
	      // factor Xb = L * Lt
	      // solve system of equations

	      //Eq_Sol sols = solve_eq(Q->X,A,C,theta);

	      Matrix *D0 = create_matrix(N,N, NULL);

	      //set up
	      //Matrix U = mat_sub(Q->X,scal_div(mat_mul(mat_mul(Q->X,C),Q->X),theta));
	      Matrix *U10 = create_matrix(N,N, NULL);
	      for (int v0 = 0; v0 < U10->rows; ++v0) {
				  for (int j0 = 0; j0 < U10->cols; ++j0) {
				    for (int k0 = 0; k0 < U10->cols; ++k0) {
							U10->m[v0*(U10->cols)+j0] += get(v0,k0,Q->X) * get(k0,j0,C);
				    }
				  }
	      }

	      Matrix *U0 = create_matrix(N,N, NULL);
	      for (int vv0 = 0; vv0 < U0->rows; ++vv0) {
				  for (int jj0 = 0; jj0 < U0->cols; ++jj0) {
				    for (int kk0 = 0; kk0 < U0->cols; ++kk0) {
							U0->m[vv0*(U0->cols)+jj0] += get(vv0,kk0,U10) * get(kk0,jj0,Q->X);
				    }
				  }
	      }

	      for (int ie0 = 0; ie0 < U0->rows*U0->cols; ie0++){
					U0->m[ie0] = Q->X->m[ie0] - (U0->m[ie0] / theta);
	      }

	     double K0[500] = {0};
	     int Krows0 = 1;
	     int Kcols0 = U0->cols*U0->rows;

	     for (int id0=0; id0<U0->rows*U0->cols; id0++){
					K0[id0] = U0->m[id0];
	      }
	      Kcols0 += M;


	      Matrix *QQ0 = create_matrix(N*N,M, NULL);
	      Matrix *nX0 = create_matrix(N,N, NULL);//scal_mul(Q->X,-1);
	      for (int ib0 = 0; ib0 < Q->X->rows*Q->X->cols; ib0++){
					nX0->m[ib0] = Q->X->m[ib0] * -1;
	      }

	      int AQrowsz = N;
	      int AQcolsz = N;

	      //for(int a = 0; a < A->len; a++){
	      //AQ->m0 = scal_div(mat_mul(mat_mul(nX,AQ->m0),Q->X),theta);

        Matrix_List *ATz = create_matrix_list(M, N, NULL);

	      for (int vb0 = 0; vb0 < N; ++vb0) {
				  for (int jb0 = 0; jb0 < N; ++jb0) {
				    for (int kb0 = 0; kb0 < N; ++kb0) {
              for (int i = 0; i < M; i++){
                ATz->m[i]->m[vb0*(AQcolsz)+jb0] += get(vb0,kb0,nX0) * get(kb0,jb0,A->m[i]);
              }
							//AT0z->m[vb0*(AQcolsz)+jb0] += get(vb0,kb0,nX0) * get(kb0,jb0,A->m0);
							//AT1z->m[vb0*(AQcolsz)+jb0] += get(vb0,kb0,nX0) * get(kb0,jb0,A->m1);;
				    }
				  }
	      }

        Matrix_List *ATTz = create_matrix_list(M, N, NULL);


	      for (int va0 = 0; va0 < N; ++va0) {
				  for (int ja0 = 0; ja0 < N; ++ja0) {
				    for (int ka0 = 0; ka0 < N; ++ka0) {
              for (int i = 0; i < M; i++){
                ATTz->m[i]->m[va0*(AQcolsz)+ja0] += get(va0,ka0,ATz->m[i]) * get(ka0,ja0,Q->X);
              }
							//AT00z->m[va0*(AQcolsz)+ja0] += get(va0,ka0,AT0z) * get(ka0,ja0,Q->X);
							//AT11z->m[va0*(AQcolsz)+ja0] += get(va0,ka0,AT1z) * get(ka0,ja0,Q->X);
				    }
				  }
	      }

	      for (int ia0 = 0; ia0 < AQrowsz*AQcolsz; ia0++){
          for (int i = 0; i < M; i++){
            ATTz->m[i]->m[ia0] = ATTz->m[i]->m[ia0] / theta;
          }
					//AT00z->m[ia0] = AT00z->m[ia0] / theta;
					//AT11z->m[ia0] = AT11z->m[ia0] / theta;
	      }
	      //AQ->m0 = flatten(AQ->m0);
	      AQcolsz = AQcolsz*AQrowsz;
	      AQrowsz = 1;

	      //AQ->m1 = scal_div(mat_mul(mat_mul(nX,AQ->m1),Q->X),theta);
	      //AQ->m1 = flatten(AQ->m1);

	      //}
	      for (int c0 = 0; c0 < QQ0->rows; c0++){
          for (int i = 0; i < M; i++){
            QQ0->m[c0*(QQ0->cols)+i] = get(0,c0,ATTz->m[i]);
          }
					//for (int b = 0; b < Q->cols; b++){
					//QQ0->m[c0*(QQ0->cols)+0] = get(0,c0,AT00z);
					//QQ0->m[c0*(QQ0->cols)+1] = get(0,c0,AT11z);
					//}
	      }

	      Matrix *R0 = create_matrix(M,N*N, NULL);
	      //for (int d = 0; d < R->rows; d++){
	      for (int ee0 = 0; ee0 < R0->cols; ee0++){
          for (int i = 0; i < M; i++){
            R0->m[i*(R0->cols)+ee0] = A->m[i]->m[ee0];
          }
					//R0->m[0*(R0->cols)+ee0] = A->m0->m[ee0];
					//R0->m[1*(R0->cols)+ee0] = A->m1->m[ee0];
	      }

	      Matrix *LL0 = create_matrix((QQ0->rows + R0->rows),(N*N + QQ0->cols), NULL);
              //prepend identity matrix to Q
              for (int fq0 = 0; fq0 < QQ0->rows; fq0++){
                for (int gq0 = 0; gq0 < N*N; gq0++){
                  if (fq0 == gq0){
                    LL0->m[fq0*(LL0->cols)+gq0] = 1.0;
                  }
              }
              for (int gg0 = N*N; gg0 < LL0->cols; gg0++){
                LL0->m[fq0*(LL0->cols)+gg0] = get(fq0,(gg0-N*N),QQ0);
              }
            }
            // append P, zeros
            for (int ip0 = QQ0->rows; ip0 < LL0->rows; ip0++){
              for (int jp0 = 0; jp0 < R0->cols; jp0++){
                LL0->m[ip0*(LL0->cols)+jp0] = get((ip0-QQ0->rows),jp0,R0);
              }
              for (int hp0 = R0->cols; hp0 < LL0->cols; hp0++){
                LL0->m[ip0*(LL0->cols)+hp0] = 0.0;
              }
            }

            //least sq solution
            //Big_Vector e = vectorize(K);
            Vector *e0 = create_vector(Krows0*Kcols0, NULL);
            for (int iik0 = 0; iik0<e0->len; iik0++){
              e0->v[iik0] = K0[iik0];
            }

            //Big_Vector z = LUP_solve(LL, e);
            //Big_Matrix_List LU_List = LUP_decompose_big(LL);
			      Matrix *LD0 = create_matrix(LL0->rows,LL0->cols, NULL);
			      Matrix *UD0 = create_matrix(LL0->rows,LL0->cols, NULL);
			      Matrix *PD0 = create_matrix(LL0->rows,LL0->cols, NULL);
			      int n0 = LL0->rows;

			      //pivot A P
			      for (int ld0=0; ld0<n0;ld0++){
			        for (int gd0=0; gd0<n0; gd0++){
			          if (ld0 == gd0){
			            PD0->m[ld0*(LD0->cols)+gd0] = 1.0;
			          }
			        }
			      }

						for (int yd0=0; yd0<n0; yd0++){
			        int max_j0 = yd0;
			        for (int rd0 = yd0; rd0 < n0; rd0++){
			          if (abs_val(get(rd0,yd0,LL0)) > abs_val(get(max_j0,yd0,LL0))){
			            max_j0 = rd0;
			          }
			        }

			        if (max_j0 != yd0){
			          //PD = swap_rows_big(PD,yd,max_j);
			          for (int sr0 = 0; sr0 < PD0->cols; sr0++){
			            double temp0 = get(yd0, sr0, PD0);
			            PD0->m[yd0*(PD0->cols)+sr0] = get(max_j0, sr0, PD0);
			            PD0->m[max_j0*(PD0->cols)+sr0] = temp0;
			          }
			        }
			      }

						//Big_Matrix LLp = mat_mul_big(PD,LL);
			      Matrix *LLp0 = create_matrix(PD0->rows,LL0->cols, NULL);

			      for (int vll0 = 0; vll0 < PD0->rows; ++vll0) {
			          for (int jll0 = 0; jll0 < LL0->cols; ++jll0) {
			            for (int kll0 = 0; kll0 < PD0->cols; ++kll0) {
			                LLp0->m[vll0*(LLp0->cols)+jll0] += get(vll0,kll0,PD0) * get(kll0,jll0,LL0);
			            }
			          }
			      }

						for (int vd0=0; vd0<n0; vd0++){
							LD0->m[vd0*(LD0->cols)+vd0] = 1.0;
						}

						for (int ig0=0;ig0<n0;ig0++){

							for (int je0=0;je0<n0;je0++){
								double se0;
								if (je0 <= ig0){
									se0 = 0;
									for (int ce0 = 0; ce0 < je0; ce0++){
										se0 += get(je0,ce0,LD0) * get(ce0,ig0,UD0);
									}
									UD0->m[je0*(UD0->cols)+ig0] = (get(je0,ig0,LLp0) - se0);
								}
								if (je0 >= ig0){
									se0 = 0;
									for (int ke0 = 0; ke0 < ig0; ke0++){
										se0 += get(je0,ke0,LD0) * get(ke0,ig0,UD0);
									}
									LD0->m[je0*(LD0->cols)+ig0] = (get(je0,ig0,LLp0) - se0) / get(ig0,ig0,UD0);
								}
							}
						}

						Matrix *I0 = create_matrix(LL0->rows,LL0->cols, NULL);

			      for (int t0 = 0; t0 < I0->rows; t0++){
			        for (int u0 = 0; u0 < I0->cols; u0++){
			          if (t0 == u0){
			            I0->m[t0*(I0->cols)+u0] = 1.0;
			          }
			        }
			      }

			      //Big_Matrix_List LU_List = {2, , PD};


			      // end decomp

						//Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
						Matrix *LU0 = create_matrix(LD0->rows,LD0->cols, NULL);
						for (int tu0 = 0; tu0 < I0->rows; tu0++){
			        for (int uu0 = 0; uu0 < I0->cols; uu0++){
			          LU0->m[tu0*(LU0->cols)+uu0] = get(tu0,uu0,LD0) + get(tu0,uu0,UD0) - get(tu0,uu0,I0);
			        }
			       }

						 //Big_Vector z = vec_mul_big(PD,e);
			      Vector *z0 = create_vector(PD0->cols, NULL);

			      for (int iz0 = 0; iz0 < PD0->rows; ++iz0) {
			        for (int kz0 = 0; kz0 < PD0->cols; ++kz0) {
			            z0->v[iz0] += get(iz0,kz0,PD0) * e0->v[kz0];
			        }
			      }

			      // forward substitute
			      for (int a0 = 0; a0 < n0-1; a0++){
			        for (int b0 = a0+1; b0 < n0; b0++){
			          z0->v[b0] = z0->v[b0] - (get(b0,a0,LU0) * z0->v[a0]);
			        }
			      }

			      // backwards substitute
			      for (int i0 = (n0-1); i0 >= 0; i0--){
			        if (abs_val(get(i0,i0,LU0)) < epsilon){
			          valid = 0;
			        }
			        z0->v[i0] = z0->v[i0] / get(i0,i0,LU0);
			        for (int j0 = 0; j0 < i0; j0++){
			          z0->v[j0] = z0->v[j0] - (get(j0,i0,LU0) * z0->v[i0]);
			        }
			      }

			      //end LUP solve

            //printf("Z");
            //print_vec_b(z0);

						//norm - for res error;
						//Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
						//double err = norm_vec_big(res);
						double errs = norm_circ(e0, LL0, z0);

						for (int l0 = 0; l0 < N*N; l0++){
			        D0->m[l0] = z0->v[l0];
			      }

			      //D = scal_mul(mat_add(D,transpose(D)),0.5);
			      Matrix *T0 = D0;
			      for (int it0 = 0; it0 < T0->rows; it0++) {
			        for (int jt0 = 0; jt0 < T0->cols; jt0++) {
			            T0->m[it0*(T0->cols)+jt0] = get(jt0,it0,D0);
			        }
			      }
			      for (int kt0 = 0; kt0 < D0->rows*D0->cols; kt0++){
			        D0->m[kt0] = (D0->m[kt0] + T0->m[kt0]) * 0.5;
			      }


			      for (int o0 = N*N; o0 < (N*N)+M; o0++){
			        new_y->v[o0-(N*N)] = z0->v[o0];
			      }

						// end solve_eq


	      if (errs <= 1e-2){

	        //4-> update all values
	        Q->y = new_y;
          //printf("Y");
          //print_vec(new_y);



	        double t = 1.0;

	        //Matrix XDT = mat_add(Q->X,scal_mul(sols->D,t));
					Matrix *XDT = create_matrix(N,N, NULL);
					for (int xy = 0; xy < D0->rows*D0->cols; xy++){
						XDT->m[xy] = Q->X->m[xy] + (D0->m[xy] * t);
					}
	        while (!psd(XDT)){
            //printf("NOT PSD\n");
	          t = alpha * t;
	          //XDT = mat_add(Q->X,scal_mul(sols->D,t));
						for (int xz = 0; xz < D0->rows*D0->cols; xz++){
							XDT->m[xz] = Q->X->m[xz] + (D0->m[xz] * t);
						}
	        }

	        if (theta >= 1e-4){
	          Q->X = XDT;
	        }
	      }


			}


		} // end if
	  return Q;
	}

/*Problem infeasibility_pre(Problem P){

  for (int i=0; i<M; i++){
    Matrix Ai = P->A->mats[i];
    if (psd(Ai) && P->b->v[i] < 0){
      P->feasible = 0;

      break;
    }
    if (nd(Ai) && P->b->v[i] > 0){
      P->feasible = 0;

      break;
    }

  }

}*/

/*
int sdp_check(Matrix C, Matrix X, Matrix_List A, Vector b, Vector y, int feasible){

  int solved = 1;

  if (feasible){

    // (X) feasible
    solved = solved && psd(X);

    //for (int i = 0; i<P->A->len; i++){
    solved = solved && d_equal(dot(A->m0, X),b->v[0]);
    solved = solved && d_equal(dot(A->m1, X),b->v[1]);
    //}

    // (y,S) feasible
    //Matrix S = mat_sub(C, mat_comb(y,A)); // sum from 1 to m of yi*Ai
    //comb - Matrix sum = scal_mul(A->m0, y->v[0]);
    //sum = mat_add(sum,scal_mul(A->m1, y->v[1]));
    Matrix S = {N,N,{0}};
    for (int i = 0; i < S->rows*S->cols; i++){
      S->m[i] = C->m[i] - ((A->m0->m[i] * y->v[0]) + (A->m1->m[i] * y->v[1]));
    }

    solved = solved && psd(S);

    // C*X - sum(yi*bi) = 0 (duality gap = 0)
    double gap = dot(S,X); //dot(P->C,P->X) - vec_comb(P->y,P->b);
    solved = solved && (d_equal_ep(gap,0.0,1e-2));

    ////printf("gap %6f\n", gap);

  } else { //infeasibly

    // X doesn't fit problem
    //for (int f = 0; f<P->A->len; f++){
      if (!(d_equal(dot(A->m0, X),b->v[0]))){
        solved = 1;
      }
      if (!(d_equal(dot(A->m1, X),b->v[1]))){
        solved = 1;
      }
    //}

    //trivial cert - sys of alt
    if (d_equal(vec_vec_mul(P->y,P->b),-1.0)){
      //for (int j = 0; j < P->A->len; j++){
        infeas = infeas && (psd(scal_mul(get_mat(P->A,j),P->y->v[j])));
      //}

    }
  }
  return solved;


}

Matrix sdp_gadget(Matrix C, Matrix X, Matrix_List A, Vector b) {
  Solution Q = __GADGET_compute(sdp(C,X,A,b,1.0,0.25));
  __GADGET_check(sdp_check(C, Q->X, A, b, Q->y, Q->feasible));
  return Q->X;
}
*/
