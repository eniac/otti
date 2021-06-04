
#include <stdio.h>
      #include "sdp_api.h"


      int main(void) {

        // problem

        Matrix C = {N,N,{-0.1983367,  0.54620727, 0.54620727,  0.29183634}};
        Vector b = {M,{-2.3830572764539832, 0.8521208961278653, 0, 0}};
        Matrix X = {N,N,{1.3713053, 0.16070848,0.16070848, 1.43693619}};

        Matrix A0 = {N,N,{-0.99890972, 0.14410886,0.14410886, -0.73737868}};
        Matrix A1 = {N,N,{0.14047667, -0.17714865,-0.17714865,  0.49857682}};
        Matrix_List A = {M,A0,A1};

        //Vector y = {M, {6.84191924, 8.82245222, 0,0}};
        //Matrix QX = {N,N,{2363.08764186,  1452.8059931, 1452.8059931,  10655.14262453}};
        printf("start\n");
        Problem_Box Q = sdp(C,X,A,b,1.0,0.25);
        //Matrix Q = sdp_gadget(C,X,A,b);
        //print_mat(Q.X);
        int r = sdp_check(Q, Q.feasible);
        printf("check %d\n", r);
        return 0;
      }



      fixed_point_precision_16_16 get(int i, int j, Matrix mat){
        return mat.m[i*N+j];
      }

      fixed_point_precision_16_16 get_big(int i, int j, Big_Matrix mat, int c){
        return mat.m[i*c+j];
      }

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


      void print_mat(Matrix X) {

        printf("\n-----------------\n");

        for(int i=0;i<X.rows; i++) {
          for(int j=0;j<X.cols; j++) {
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

/*      void print_mat_b(Big_Matrix X) {

        printf("\n-----------------\n");

        for(int i=0;i<X.rows; i++) {
          for(int j=0;j<X.cols; j++) {
            if (d_equal((int)(get_big(i,j,X)), (get_big(i,j,X)))) {
              printf(" %6d", (int)get_big(i,j,X));
            } else {
              printf(" %6.3f", get_big(i,j,X));
            }
          }
          printf("\n");

        }

        printf("\n----------------\n");
      }
*/
      void print_vec(Vector v) {

        printf("\n-----------------\n");


        for(int i=0;i<v.len; i++) {
          if (d_equal((int)(v.v[i]), (v.v[i]))) {
            printf(" %6d", (int)v.v[i]);
          } else {
            printf(" %6.3f", v.v[i]);
          }
          printf("\n");

        }

        printf("\n----------------\n");
      }
/*
      void print_vec_b(Big_Vector v) {

        printf("\n-----------------\n");


        for(int i=0;i<v.len; i++) {
          if (d_equal((int)(v.v[i]), (v.v[i]))) {
            printf(" %6d", (int)v.v[i]);
          } else {
            printf(" %6.3f", v.v[i]);
          }
          printf("\n");

        }

        printf("\n----------------\n");
      }
*/
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


/*
      fixed_point_precision_16_16 dot(Matrix A, Matrix B, int size){
        fixed_point_precision_16_16 s = 0;
        for (int i = 0; i <size; i++){
          for (int j=0; j<size; j++){
            s = s + (A.m[i*size+j] * B.m[i*size+j]);
          }
        }
        return s;
      }
*/
      fixed_point_precision_16_16 vec_comb(Vector a, Vector b){
        fixed_point_precision_16_16 s = 0;
        for (int i = 0; i < a.len; i++){
          s = s + (a.v[i] * b.v[i]);
        }

        return s;
      }

      Matrix scal_div(Matrix A, fixed_point_precision_16_16 s){
        for (int i = 0; i < A.rows*A.cols; i++){
          A.m[i] = A.m[i] / s;
        }
        return A;
      }

      Matrix mat_mul(Matrix A, Matrix B){
        Matrix C = {A.rows,B.cols,{0,0,0,0}};

         for (int v = 0; v < A.rows; ++v) {
            for (int j = 0; j < B.cols; ++j) {
               for (int k = 0; k < A.cols; ++k) {
                  C.m[v*(C.cols)+j] += get(v,k,A) * get(k,j,B);
               }
            }
         }
        return C;

      }
/*
      Big_Matrix mat_mul_big(Big_Matrix A, Big_Matrix B){
        Big_Matrix C = {A.rows,B.cols,{0,0,0,0,0,0,0,0,0,0}};
         for (int i = 0; i < C.rows; ++i) {
            for (int j = 0; j < C.cols; ++j) {
               C.m[i*(C.cols)+j] = 0.0;
            }
         }

         for (int v = 0; v < A.rows; ++v) {
            for (int j = 0; j < B.cols; ++j) {
               for (int k = 0; k < A.cols; ++k) {
                  C.m[v*(C.cols)+j] += get_big(v,k,A) * get_big(k,j,B);
               }
            }
         }
        return C;

      }
*/
      //TODO CHECK
      Vector vec_mul(Matrix A, Vector b){
        Vector c = {A.cols,{0,0,0,0}};

         for (int i = 0; i < A.rows; ++i) {
               for (int k = 0; k < A.cols; ++k) {
                  c.v[i] += get(i,k,A) * b.v[k];
               }
         }
        return c;

      }
/*
      //TODO CHECK
      Big_Vector vec_mul_big(Big_Matrix A, Big_Vector b){
        Big_Vector c = {A.cols,{0,0,0,0,0,0,0,0,0,0}};

         for (int i = 0; i < A.rows; ++i) {
               for (int k = 0; k < A.cols; ++k) {
                  c.v[i] += get_big(i,k,A) * b.v[k];
               }
         }
        return c;

      }
*/
      fixed_point_precision_16_16 vec_vec_mul(Vector a, Vector b){
        fixed_point_precision_16_16 c = 0.0;

         for (int i = 0; i < a.len; ++i) {
            c += a.v[i]*b.v[i];
         }

        return c;

      }

      Matrix scal_mul(Matrix A, fixed_point_precision_16_16 s){
        for (int i = 0; i < A.rows*A.cols; i++){
          A.m[i] = A.m[i] * s;
        }
        return A;
      }

      Matrix mat_add(Matrix A, Matrix B){
        Matrix C = A;
        for (int i = 0; i < A.rows*A.cols; i++){
          C.m[i] = A.m[i] + B.m[i];
        }
        return C;
      }


      Big_Matrix mat_add_big(Big_Matrix A, Big_Matrix B){
        Big_Matrix C = A;
        for (int i = 0; i < A.rows*A.cols; i++){
          C.m[i] = A.m[i] + B.m[i];
        }
        return C;
      }

      Matrix mat_sub(Matrix A, Matrix B){
        Matrix C = A;
        for (int i = 0; i < A.rows*A.cols; i++){
          C.m[i] = A.m[i] - B.m[i];
        }
        return C;
      }

      Big_Matrix mat_sub_big(Big_Matrix A, Big_Matrix B){
        Big_Matrix C = A;
        for (int i = 0; i < A.rows*A.cols; i++){
          C.m[i] = A.m[i] - B.m[i];
        }
        return C;
      }

      Matrix mat_comb(Vector y, Matrix_List A){
        Matrix sum = scal_mul(A.m0, y.v[0]);
        sum = mat_add(sum,scal_mul(A.m1, y.v[1]));
        return sum;

      }

      Matrix transpose(Matrix A){
        Matrix T = A;
        for (int i = 0; i < T.rows; i++) {
          for (int j = 0; j < T.cols; j++) {
              T.m[i*(T.cols)+j] = get(j,i,A);
          }
        }
        return T;
      }

      Vector vec_sub(Vector a, Vector b){
        Vector x = a;
        for (int i = 0; i < x.len; i++){
          x.v[i] = a.v[i] - b.v[i];
        }
        return x;
      }

      Big_Vector vec_sub_big(Big_Vector a, Big_Vector b){
        Big_Vector x = a;
        for (int i = 0; i < x.len; i++){
          x.v[i] = a.v[i] - b.v[i];
        }
        return x;
      }

      Big_Vector vectorize(Big_Matrix A){
        Big_Vector v = {A.rows*A.cols,{0,0,0,0,0,0,0,0,0,0}};
        for (int i = 0; i<v.len; i++){
          v.v[i] = A.m[i];
        }
        return v;
      }

      Big_Matrix biggify_mat(Matrix P, int rows, int cols){

        Big_Matrix A = {rows,cols,{0,0,0,0,0,0,0,0,0,0}};
        for (int i=0; i<rows*cols; i++){
          A.m[i] = P.m[i];
        }
        return A;

      }

      Vector reg_vec(Gauss_Vec g){
        Vector v = {g.len,{0,0,0,0}};
        for (int i = 0; i<v.len; i++){
          v.v[i] = g.v[i];
        }
        return v;
      }

      /*
      Vector sym_vectorize(Matrix A){
        Big_Vector v = {(A.rows * (A.rows+1))/2,{0,0,0,0,0,0,0,0,0,0}};

        int c = 0;
        for (int i = 0; i<A.rows; i++){
          for (int j = 0; j<A.cols; j++){
            if (i==j){
              v.v[c] = get(i,j,A);
              c++;
            } else if (i < j){
              v.v[c] = get(i,j,A) + get(j,i,A);
              c++;
            }
          }
        }
        return v;
      }
      */

      // matrix row manips
      Matrix swap_rows(Matrix X, int a, int b){
        for (int j = 0; j < X.cols; j++){
          fixed_point_precision_16_16 temp = get(a, j, X);
          X.m[a*(X.cols)+j] = get(b, j, X);
          X.m[b*(X.cols)+j] = temp;
        }

        return X;
      }
/*
      Big_Matrix swap_rows_big(Big_Matrix X, int a, int b){
        for (int j = 0; j < X.cols; j++){
          fixed_point_precision_16_16 temp = get_big(a, j, X);
          X.m[a*(X.cols)+j] = get_big(b, j, X);
          X.m[b*(X.cols)+j] = temp;
        }

        return X;
      }
*/
      // divide row a in mat M by scalar s
      Matrix div_row(Matrix X, int a, fixed_point_precision_16_16 s){
        for (int j = 0; j < X.cols; j++){
          X.m[a*(X.cols)+j] = get(a, j, X) / s;
        }

        return X;
      }

      Matrix flatten(Matrix X){
        X.cols = X.cols*X.rows;
        X.rows = 1;

        return X;

      }

      // subtract (row b * scalar s) from row a
      Matrix sub_multd_row(Matrix X, int a, int b, fixed_point_precision_16_16 s){
        for (int j = 0; j < X.cols; j++){
          X.m[a*(X.cols)+j] = get(a, j, X) - (get(b, j, X)*s);
        }

        return X;
      }

      fixed_point_precision_16_16 norm_circ(Big_Vector e, Big_Matrix LL, int size, Big_Vector z){
        //Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
        Big_Vector res = {size,{0,0,0,0,0,0,0,0,0,0}};

        for (int i = 0; i < size; ++i) {
              for (int k = 0; k < size; ++k) {
                res.v[i] += LL.m[i*size+k] * z.v[k];
              }
        }
      for (int j = 0; j < size; j++){
              res.v[j] = e.v[j] - res.v[j];
      }


        fixed_point_precision_16_16 err = norm_vec_big(res);
        return err;

      }

      fixed_point_precision_16_16 norm_vec_big(Big_Vector v){

        fixed_point_precision_16_16 sum = 0;
        for (int i=0; i<v.len; i++){
          sum = sum + pow_2(v.v[i]);
        }

        fixed_point_precision_16_16 r = sqrt_val(sum);
        return r;
      }

      fixed_point_precision_16_16 norm_mat_circ(Matrix L, int Lsize, Matrix D){
//Matrix I = mat_mul(mat_mul(inverse(L),D),inverse(transpose(L)));

//LI = inverse(L)
//Matrix_List LU_List = LUP_decompose(L);
Matrix LA = {Lsize,Lsize,{0,0,0,0}};
Matrix UA = {Lsize,Lsize,{0,0,0,0}};
Matrix PA = {Lsize,Lsize,{0,0,0,0}};
int n = Lsize;

//pivot A P
for (int ld=0; ld<n;ld++){
  for (int gd=0; gd<n; gd++){
    if (ld == gd){
      PA.m[ld*(Lsize)+gd] = 1.0;
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
    for (int sr = 0; sr < Lsize; sr++){
      fixed_point_precision_16_16 temp = get(yd, sr, PA);
      PA.m[yd*(Lsize)+sr] = get(max_j, sr, PA);
      PA.m[max_j*(Lsize)+sr] = temp;
    }
  }
}

//Big_Matrix LLp = mat_mul_big(PD,LL);
Matrix LLp = {Lsize,Lsize,{0,0,0,0}};

for (int vll = 0; vll < Lsize; ++vll) {
    for (int jll = 0; jll < Lsize; ++jll) {
      for (int kll = 0; kll < Lsize; ++kll) {
          LLp.m[vll*(Lsize)+jll] += get(vll,kll,PA) * get(kll,jll,L);
      }
    }
}

for (int vd=0; vd<n; vd++){
  LA.m[vd*(Lsize)+vd] = 1.0;
}

for (int ig=0;ig<n;ig++){

  for (int je=0;je<n;je++){
    fixed_point_precision_16_16 se;
    if (je <= ig){
      se = 0;
      for (int ce = 0; ce < je; ce++){
        se += get(je,ce,LA) * get(ce,ig,UA);
      }
      UA.m[je*(Lsize)+ig] = (get(je,ig,LLp) - se);
    }
    if (je >= ig){
      se = 0;
      for (int ke = 0; ke < ig; ke++){
        se += get(je,ke,LA) * get(ke,ig,UA);
      }
      LA.m[je*(Lsize)+ig] = (get(je,ig,LLp) - se) / get(ig,ig,UA);
    }
  }
}


Matrix IDT = {Lsize,Lsize,{0,0,0,0}};
for (int t = 0; t < Lsize; t++){
  for (int u = 0; u < Lsize; u++){
    if (t == u){
      IDT.m[t*(Lsize)+u] = 1.0;
    }
  }
}

//Big_Matrix_List LU_List = {2, , PD};

//Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
Matrix LU = {Lsize,Lsize,{0,0,0,0}};
for (int tu = 0; tu < N; tu++){
  for (int uu = 0; uu < N; uu++){
    LU.m[tu*(Lsize)+uu] = get(tu,uu,LA) + get(tu,uu,UA) - get(tu,uu,IDT);
  }
}

// end decomp

Matrix P = PA;
Matrix LI = LU;

for (int i = 0; i<n; i++){
  Big_Vector bb = {n, {0,0,0,0,0,0,0,0,0,0}};
  for (int j = 0; j<n; j++){
    if (i==j){
      bb.v[j] = 1.0;
    }
  }

  //b = vec_mul_big(biggify_mat(P,P.rows,P.cols),b);
  Big_Vector b = {Lsize,{0,0,0,0,0,0,0,0,0,0}};

  for (int iq = 0; iq < Lsize; ++iq) {
        for (int kq = 0; kq < Lsize; ++kq) {
            b.v[iq] += get(iq,kq,P) * bb.v[kq];
        }
  }

    // forward substitute
  for (int d = 0; d < n-1; d++){
    for (int e = d+1; e < n; e++){
      b.v[e] = b.v[e] - (get(e,d,LU) * b.v[d]);
    }
  }

  // backwards substitute
  for (int f = (n-1); f >= 0; f--){
    if (abs_val(get(f,f,LU)) < epsilon){
      valid = 0;
    }
    b.v[f] = b.v[f] / get(f,f,LU);
    for (int h = 0; h < f; h++){
      b.v[h] = b.v[h] - (get(h,f,LU) * b.v[f]);
    }
  }

  for (int k = 0; k < n; k++){
      LI.m[k*(Lsize)+i] = b.v[k];//TODO
  }
}

//LT = transpose(L)
Matrix LT = L;
for (int itt = 0; itt < Lsize; itt++) {
  for (int jtt = 0; jtt < Lsize; jtt++) {
      LT.m[itt*(Lsize)+jtt] = get(jtt,itt,L);
  }
}

//LTI = inverse(L.T)
//Matrix_List LU_List = LUP_decompose(LT);
Matrix LB = {Lsize,Lsize,{0,0,0,0}};
Matrix UB = {Lsize,Lsize,{0,0,0,0}};
Matrix PB = {Lsize,Lsize,{0,0,0,0}};

//pivot A P
for (int ld2=0; ld2<n;ld2++){
  for (int gd2=0; gd2<n; gd2++){
    if (ld2 == gd2){
      PB.m[ld2*(Lsize)+gd2] = 1.0;
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
    for (int sr2 = 0; sr2 < Lsize; sr2++){
      fixed_point_precision_16_16 temp = get(yd2, sr2, PB);
      PB.m[yd2*(Lsize)+sr2] = get(max_j2, sr2, PB);
      PB.m[max_j2*(Lsize)+sr2] = temp;
    }
  }
}

//Big_Matrix LLp = mat_mul_big(PD,LL);
Matrix LLpp = {Lsize,Lsize,{0,0,0,0}};

for (int vll2 = 0; vll2 < N; ++vll2) {
    for (int jll2 = 0; jll2 < N; ++jll2) {
      for (int kll2 = 0; kll2 < N; ++kll2) {
          LLpp.m[vll2*(Lsize)+jll2] += get(vll2,kll2,PB) * get(kll2,jll2,LT);
      }
    }
}

for (int vd2=0; vd2<n; vd2++){
  LB.m[vd2*(Lsize)+vd2] = 1.0;
}

for (int ig2=0;ig2<n;ig2++){

  for (int je2=0;je2<n;je2++){
    fixed_point_precision_16_16 se2;
    if (je2 <= ig2){
      se2 = 0;
      for (int ce2 = 0; ce2 < je2; ce2++){
        se2 += get(je2,ce2,LB) * get(ce2,ig2,UB);
      }
      UB.m[je2*(Lsize)+ig2] = (get(je2,ig2,LLpp) - se2);
    }
    if (je2 >= ig2){
      se2 = 0;
      for (int ke2 = 0; ke2 < ig2; ke2++){
        se2 += get(je2,ke2,LB) * get(ke2,ig2,UB);
      }
      LB.m[je2*(Lsize)+ig2] = (get(je2,ig2,LLpp) - se2) / get(ig2,ig2,UB);
    }
  }
}


//Big_Matrix_List LU_List = {2, , PD};

//Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
Matrix LU2 = {Lsize,Lsize,{0,0,0,0}};
for (int tu2 = 0; tu2 < N; tu2++){
  for (int uu2 = 0; uu2 < N; uu2++){
    LU2.m[tu2*(Lsize)+uu2] = get(tu2,uu2,LB) + get(tu2,uu2,UB) - get(tu2,uu2,IDT);
  }
}

// end decomp

Matrix LTI = LU2;

for (int i2 = 0; i2<n; i2++){
  Big_Vector bb2 = {n, {0,0,0,0,0,0,0,0,0,0}};
  for (int j2 = 0; j2<n; j2++){
    if (i2==j2){
      bb2.v[j2] = 1.0;
    }
  }

  //b = vec_mul_big(biggify_mat(P,P.rows,P.cols),b);
  Big_Vector b2 = {Lsize,{0,0,0,0,0,0,0,0,0,0}};

  for (int i3 = 0; i3 < Lsize; ++i3) {
        for (int k3 = 0; k3 < Lsize; ++k3) {
            b2.v[i3] += get(i3,k3,PB) * bb2.v[k3];
        }
  }

    // forward substitute
  for (int d3 = 0; d3 < n-1; d3++){
    for (int e3 = d3+1; e3 < n; e3++){
      b2.v[e3] = b2.v[e3] - (get(e3,d3,LU2) * b2.v[d3]);
    }
  }

  // backwards substitute
  for (int f3 = (n-1); f3 >= 0; f3--){
    if (abs_val(get(f3,f3,LU2)) < epsilon){
      valid = 0;
    }
    b2.v[f3] = b2.v[f3] / get(f3,f3,LU2);
    for (int h3 = 0; h3 < f3; h3++){
      b2.v[h3] = b2.v[h3] - (get(h3,f3,LU2) * b2.v[f3]);
    }
  }

  for (int k4 = 0; k4 < n; k4++){
      LTI.m[k4*(Lsize)+i2] = b2.v[k4];//TODO
  }
}
// end inverse

//I = mul: (L.I * D) * L.T.I
Matrix LID = {Lsize,Lsize,{0,0,0,0}};
for (int v = 0; v < N; ++v) {
  for (int jd = 0; jd < N; ++jd) {
      for (int k = 0; k < N; ++k) {
        LID.m[v*(Lsize)+jd] += get(v,k,LI) * get(k,jd,D);
      }
  }
}
Matrix I = {Lsize,Lsize,{0,0,0,0}};
for (int vi = 0; vi < N; ++vi) {
  for (int ji = 0; ji < N; ++ji) {
      for (int ki = 0; ki < N; ++ki) {
        I.m[vi*(Lsize)+ji] += get(vi,ki,LID) * get(ki,ji,LTI);
      }
  }
}

fixed_point_precision_16_16 stop = norm_mat(I);

return stop;
}


      fixed_point_precision_16_16 norm_mat(Matrix X){
        fixed_point_precision_16_16 sum = 0.0;
        for (int i=0; i<X.rows; i++){
          for (int j=0; j<X.cols; j++){
            sum = sum + pow_2(get(i,j,X));
          }
        }
        fixed_point_precision_16_16 r = sqrt_val(sum);
        return r;
      }

      // gauss elim for Ax = b, input [A b]
      /*Gauss_Vec gauss_solve(Matrix A, Vector b){
          Gauss_Vec x;
          x.len = A.cols;
          x.sols = 1;

          Matrix X;
          X.cols = A.cols + 1;
          X.rows = A.rows;
          for (int i = 0; i < X.rows; i++){
            for (int j = 0; j < X.cols; j++){
              if (j<A.cols){
                X.m[i*(X.cols)+j] = get(i,j,A);
              } else {
                X.m[i*(X.cols)+j] = b.v[i];
              }
            }

          }
          printf("A");
          print_mat(A);
          print_vec(b);
          print_mat(X);

          int u = A.cols;

          // reduction into r.e.f.
          int singular = -1;
          for (int k=0; k<u; k++)
          {
              // Initialize maximum value and index for pivot
              int i_max = k;
              int v_max = get(i_max,k,X);

              // find greater amplitude for pivot if any
              for (int i = k+1; i < u; i++){
                  if (abs_val(get(i,k,X) > v_max)){
                      v_max = get(i,k,X);
                      i_max = i;

                  }
              }

              if (!get(k,i_max,X)) {
                  singular = k; // Matrix is singular
                  break;
              }
              if (i_max != k) {
                  swap_rows(X, k, i_max);
              }

              for (int i=k+1; i<u; i++)
              {
                  //factor f to set current row kth element to 0,
                   // and subsequently remaining kth column to 0
                  fixed_point_precision_16_16 f = get(i,k,X)/get(k,k,X);

                  for (int j=k+1; j<=u; j++){
                      X.m[i*(X.cols)+j] -= get(k,j,X)*f;
                  }
                  // filling lower triangular matrix with zeros
                  X.m[i*(X.cols)+k] = 0;
              }

          }

          if (singular != -1)
          {
              printf("Singular Matrix.\n");

              if (d_equal(get(singular,u,X),0.0)){
                  x.sols = -1;
                  printf("Inconsistent System.");
              } else {
                  printf("May have infinitely many "
                         "solutions %d.", singular);
                  x.sols = 0;

                  //pick one


              }
              return x;
          }

         // get solution to system with backward substitution

          for (int i = u-1; i >= 0; i--)
          {
              x.v[i] = get(i,u,X);

              for (int j=i+1; j<u; j++)
              {

                  x.v[i] -= get(i,j,X)*x.v[j];
              }
              x.v[i] = x.v[i]/get(i,i,X);
          }

          printf("\nSolution for the system:\n");
          return x;
      } */

      /*Vector least_squares(Matrix A, Vector b){

        Matrix AT = transpose(A);

        Matrix R = mat_mul(AT,A);
        Vector ATb = vec_mul(AT,b);

        //append
        int total = R.rows*R.cols;
        R.cols += 1;
        for (int i=total; i < total+ATb.len; i++){
          R.m[i] = ATb.v[i-total];
        }

        int lead = 0;
        //row reduce
        for (int r=0; r < R.rows; r++){

          int i = r;
          while (get(i, lead, R) == 0) {
            i = i + 1;
            if (R.rows == i){
              i = r;
              lead = lead + 1;
              if (R.cols == lead){
                break;
              }
            }
          }

           R = swap_rows(R, i, r);
          if (!d_equal(get(r, lead, R),0.0)) {
            R = div_row(R, r, get(r, lead, R));
          }

          for (int j = 0; j < R.rows; j++){

            if (j != r){
              R = sub_multd_row(R, j, r, get(j, lead, R));
            }
          }

          lead = lead + 1;

        }


        // get solution
        Vector x = b;
        for (int i=0; i < R.rows; i++){
          x.v[i] = get(i,(R.cols-1),R);
        }

        return x;
      }*/



/*

      Big_Matrix_List LUP_decompose_big(Big_Matrix A){

        Big_Matrix L = {A.rows,A.cols,{0,0,0,0,0,0,0,0,0,0}};
        Big_Matrix U = {A.rows,A.cols,{0,0,0,0,0,0,0,0,0,0}};
        Big_Matrix P = {A.rows,A.cols,{0,0,0,0,0,0,0,0,0,0}};
        int n = A.rows;

        //pivot A P
        for (int l=0; l<n;l++){
          for (int g=0; g<n; g++){
            P.m[l*(L.cols)+g] = (l == g);
          }
        }

        for (int y=0; y<n; y++){
          int max_j = y;
          for (int r = y; r < n; r++){
            if (abs_val(get_big(r,y,A)) > abs_val(get_big(max_j,y,A))){
              max_j = r;
            }
          }

          if (max_j != y){
            P = swap_rows_big(P,y,max_j);
          }
        }

        Big_Matrix Ap = mat_mul_big(P,A);
  printf("REAL AP");
  print_mat_b(Ap);

        for (int v=0; v<n; v++){
          L.m[v*(L.cols)+v] = 1.0;
        }

        for (int i=0;i<n;i++){

          for (int j=0;j<n;j++){
            fixed_point_precision_16_16 s;
            if (j <= i){
        s = 0;
        for (int c = 0; c < j; c++){
          s += get_big(j,c,L) * get_big(c,i,U);
        }
        U.m[j*(U.cols)+i] = (get_big(j,i,Ap) - s);
      }
      if (j >= i){
        s = 0;
        for (int k = 0; k < i; k++){
          s += get_big(j,k,L) * get_big(k,i,U);
        }
        L.m[j*(L.cols)+i] = (get_big(j,i,Ap) - s) / get_big(i,i,U);
            }
          }
        }

  printf("REAL B4I");
  print_mat_b(L);
  print_mat_b(U);

        Big_Matrix I = {A.rows,A.cols,{0,0,0,0,0,0,0,0,0,0}};

        for (int t = 0; t < I.rows; t++){
          for (int u = 0; u < I.cols; u++){
            if (t == u){
              I.m[t*(I.cols)+u] = 1.0;
            }
          }
        }

        Big_Matrix_List LU_List = {2, mat_sub_big(mat_add_big(L,U),I), P};

        return LU_List;

      }
*/
      Matrix_List LUP_decompose(Matrix A){

        Matrix L = {A.rows,A.cols,{0,0,0,0}};
        Matrix U = {A.rows,A.cols,{0,0,0,0}};
        Matrix P = {A.rows,A.cols,{0,0,0,0}};
        int n = A.rows;

        //pivot A P
        for (int l=0; l<n;l++){
          for (int g=0; g<n; g++){
            P.m[l*(L.cols)+g] = (l == g);
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
          L.m[v*(L.cols)+v] = 1.0;
        }

        for (int i=0;i<n;i++){

          for (int j=0;j<n;j++){
            fixed_point_precision_16_16 s;
            if (j <= i){
              s = 0;
              for (int c = 0; c < j; c++){
                s += get(j,c,L) * get(c,i,U);
              }
              U.m[j*(U.cols)+i] = (get(j,i,Ap) - s);
            }
            if (j >= i){
              s = 0;
              for (int k = 0; k < i; k++){
                s += get(j,k,L) * get(k,i,U);
              }
              L.m[j*(L.cols)+i] = (get(j,i,Ap) - s) / get(i,i,U);
            }
          }
        }

        Matrix I = {A.rows,A.cols,{0,0,0,0}};

        for (int t = 0; t < I.rows; t++){
          for (int u = 0; u < I.cols; u++){
            if (t == u){
              I.m[t*(I.cols)+u] = 1.0;
            }
          }
        }

        Matrix_List LU_List = {2, mat_sub(mat_add(L,U),I), P};

        return LU_List;


      }

/*

      Big_Vector LUP_solve(Big_Matrix A, Big_Vector b){

        Big_Matrix_List LU_List = LUP_decompose_big(A);

        Big_Matrix LU = LU_List.m0;
        Big_Matrix P = LU_List.m1;

        int n = LU.rows;

        Big_Vector f = vec_mul_big(P,b);

        // forward substitute
        for (int a = 0; a < n-1; a++){
          for (int b = a+1; b < n; b++){
            f.v[b] = f.v[b] - (get_big(b,a,LU) * f.v[a]);
          }
        }

        // backwards substitute
        for (int i = (n-1); i >= 0; i--){
          if (abs_val(get_big(i,i,LU)) < epsilon){
            valid = 0;
          }
          f.v[i] = f.v[i] / get_big(i,i,LU);
          for (int j = 0; j < i; j++){
            f.v[j] = f.v[j] - (get_big(j,i,LU) * f.v[i]);
          }
        }

        return f; //TODO CHANGE

      }
*/

      /*Eq_Sol solve_eq(Matrix X, Matrix_List A, Matrix C, fixed_point_precision_16_16 theta){

        Matrix D = {N,N,{0,0,0,0}};
        Vector y = {M, {0,0,0,0}};

        //set up
        Matrix U = mat_sub(X,scal_div(mat_mul(mat_mul(X,C),X),theta));

        Big_Matrix K = biggify_mat(flatten(U),U.rows,U.cols);
        int total = U.rows*U.cols;
        for (int i=total; i < total+M; i++){
          K.m[i] = 0.0;
        }
        K.cols += M;


        Big_Matrix Q = {N*N,M,{0,0,0,0,0,0,0,0,0,0}};
        Matrix nX = scal_mul(X,-1);

        Matrix_List AQ = A;

        //for(int a = 0; a < A.len; a++){
        AQ.m0 = scal_div(mat_mul(mat_mul(nX,AQ.m0),X),theta);
        AQ.m0 = flatten(AQ.m0);

        AQ.m1 = scal_div(mat_mul(mat_mul(nX,AQ.m1),X),theta);
        AQ.m1 = flatten(AQ.m1);

        //}
        for (int c = 0; c < Q.rows; c++){
          //for (int b = 0; b < Q.cols; b++){
          Q.m[c*(Q.cols)+0] = get(0,c,AQ.m0);
          Q.m[c*(Q.cols)+1] = get(0,c,AQ.m1);
          //}
        }


        Big_Matrix R = {M,N*N,{0,0,0,0,0,0,0,0,0,0}};
        //for (int d = 0; d < R.rows; d++){
        for (int e = 0; e < R.cols; e++){
          R.m[0*(R.cols)+e] = A.m0.m[e];
        }
        for (int e = 0; e < R.cols; e++){
          R.m[1*(R.cols)+e] = A.m1.m[e];
        }


        Big_Matrix L = {(Q.rows + R.rows),(N*N + Q.cols),{0,0,0,0,0,0,0,0,0,0}};
        //prepend identity matrix to Q
        for (int f = 0; f < Q.rows; f++){
          for (int g = 0; g < N*N; g++){
            L.m[f*(L.cols)+g] = (f == g);
          }
          for (int g = N*N; g < L.cols; g++){
            L.m[f*(L.cols)+g] = get_big(f,(g-N*N),Q);
          }
        }
  // append P, zeros
        for (int i = Q.rows; i < L.rows; i++){
          for (int j = 0; j < R.cols; j++){
            L.m[i*(L.cols)+j] = get_big((i-Q.rows),j,R);
          }
          for (int h = R.cols; h < L.cols; h++){            print_mat(A);
          print_vec(b);
          print_mat(X);

          int u = A.cols;

          // reduction into r.e.f.
          int singular = -1;
          for (int k=0; k<u; k++)
          {
              // Initialize maximum value and index for pivot
              int i_max = k;
              int v_max = get(i_max,k,X);

              // find greater amplitude for pivot if any
              for (int i = k+1; i < u; i++){
                  if (abs_val(get(i,k,X) > v_max)){
                      v_max = get(i,k,X);
                      i_max = i;

                  }
              }

              if (!get(k,i_max,X)) {
                  singular = k; // Matrix is singular
                  break;
              }
              if (i_max != k) {
                  swap_rows(X, k, i_max);
              }

            L.m[i*(L.cols)+h] = 0.0;
          }
        }

        //least sq solution
        Big_Vector e = vectorize(K);

        Big_Vector z = LUP_solve(L, e);


        //norm - for res error;
        Big_Vector res = vec_sub_big(e,vec_mul_big(L,z));
        fixed_point_precision_16_16 err = norm_vec_big(res);

        //clean up D, y

        for (int l = 0; l < N*N; l++){
          D.m[l] = z.v[l];
        }

        D = scal_mul(mat_add(D,transpose(D)),0.5);

        for (int o = N*N; o < (N*N)+M; o++){
          y.v[o-(N*N)] = z.v[o];
        }



        return sol;
      }*/


      Matrix cholesky(Matrix X){
        Matrix R = {N,N,{0,0,0,0}};

        for (int i = 0; i < N; i++) {
          for (int j = 0; j < (i+1); j++) {
            fixed_point_precision_16_16 s = 0.0;

            for (int k = 0; k < j; k++) {
              s = s + R.m[i*(R.cols)+k] * R.m[j*(R.cols)+k];
            }

            if (i == j){
              fixed_point_precision_16_16 to_root = get(i,i,X) - s;
              R.m[i*(R.cols)+j] = sqrt_val(to_root);
            } else {
              R.m[i*(R.cols)+j] = (fixed_point_precision_16_16)1.0 / get(j,j,R) * (get(i,j,X) - s);
            }
          }
        }
        return R;
      }


      int psd(Matrix X){ // X is psd <-> Cholesky decomposable

        int r = 1;

        int n = N;
        //fixed_point_precision_16_16 R[4] = {0,0,0,0};
        fixed_point_precision_16_16 r00 = 0.0;
        fixed_point_precision_16_16 r01 = 0.0;
        fixed_point_precision_16_16 r10 = 0.0;
        fixed_point_precision_16_16 r11 = 0.0;

        //i = 0
          //j=0
        fixed_point_precision_16_16 s = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 to_root = X.m[0] - s;
        if (to_root < (fixed_point_precision_16_16)(0.0)){
          r = 0;
        } else {
          fixed_point_precision_16_16 root = sqrt_val(to_root);
          r00 = root;
        }


        //i = 1
          //j=0
        s = (fixed_point_precision_16_16)0.0;
        r10 = (fixed_point_precision_16_16)(1.0) / r00 * (X.m[2] - s);


          //j=1
        s = r10 * r10;
        to_root = X.m[3] - s;
        if (to_root < (fixed_point_precision_16_16)(0.0)){
                r = 0;
         } else {
                fixed_point_precision_16_16 root = sqrt_val(to_root);
                r11 = root;
         }

/*
        for (int i = 0; i < n; i++) {
          for (int j = 0; j < (i+1); j++) {
            fixed_point_precision_16_16 s = 0.0;

            for (int k = 0; k < j; k++) {
              int ik = i*n+k;
              int jk = j*n+k;
              s = s + R[ik] * R[jk];
            }

            int ij = i*n+j;
            if (i == j){
              fixed_point_precision_16_16 to_root = X.m[ij] - s;
              if (to_root < (fixed_point_precision_16_16)(0.0)){
                r = 0;
              } else {
                fixed_point_precision_16_16 root = sqrt_val(to_root);
                R[ij] = root;
              }
            } else {
              int jj = j*n+j;
              R[ij] = (fixed_point_precision_16_16)(1.0) / R[jj] * (X.m[ij] - s);
            }
          }
        }*/
        return r;
      }
/*
      Matrix inverse(Matrix X){

        Matrix_List LU_List = LUP_decompose(X);

        Matrix LU = LU_List.m0;

        Matrix P = LU_List.m1;
        Matrix I = LU;
        int n = LU.rows;

        for (int i = 0; i<n; i++){
          Big_Vector b = {n, {0,0,0,0,0,0,0,0,0,0}};
          for (int j = 0; j<n; j++){
            if (i==j){
              b.v[j] = 1.0;
            } else {
              b.v[j] = 0.0;
            }
          }

          b = vec_mul_big(biggify_mat(P,P.rows,P.cols),b);

            // forward substitute
          for (int d = 0; d < n-1; d++){
            for (int e = d+1; e < n; e++){
              b.v[e] = b.v[e] - (get(e,d,LU) * b.v[d]);
            }
          }

          // backwards substitute
          for (int f = (n-1); f >= 0; f--){
            if (abs_val(get(f,f,LU)) < epsilon){
              valid = 0;
            }
            b.v[f] = b.v[f] / get(f,f,LU);
            for (int h = 0; h < f; h++){
              b.v[h] = b.v[h] - (get(h,f,LU) * b.v[f]);
            }
          }

          for (int k = 0; k < n; k++){
              I.m[k*(I.cols)+i] = b.v[k];//TODO
          }
        }
        return I;
      }
*/


      Matrix matrixize(Vector a, int rows, int cols){

        Matrix X;
        X.rows = rows;
        X.cols = cols;

        for (int i = 0; i < a.len; i++){
          X.m[i] = a.v[i];

        }

        return X;

      }


      /*Matrix make_X0(Problem P){

        Matrix H;
        H.rows = P.A.len;
        H.cols = P.A.mats[0].rows * (P.A.mats[0].rows+1) /2;

        for (int i = 0; i<P.A.len; i++){
          Vector ai = sym_vectorize(P.A.mats[i]);

          for (int j=0; j < H.cols; j++){
            H.m[i*(H.cols)+j] = ai.v[j];
          }
        }

        Gauss_Vec x = gauss_solve(H, P.b);

        if (x.sols == 1){
          return matrixize(reg_vec(x), P.X.rows, P.X.cols);
        } else if (x.sols == -1){

        } else { //inifite TODO

        }



        return  matrixize(reg_vec(x), P.X.rows, P.X.cols);

      } */

      Problem_Box sdp(Matrix C, Matrix Xp, Matrix_List A, Vector b, fixed_point_precision_16_16 theta, fixed_point_precision_16_16 beta){

        // Reference: http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-251j-introduction-to-mathematical-programming-fall-2009/readings/MIT6_251JF09_SDP.pdf

        int p = psd(Xp);
        Matrix X = Xp;
        Vector new_y = {M,{0,0,0,0}};


        fixed_point_precision_16_16 dot_d1 = 0;
        fixed_point_precision_16_16 dot_d2 = 0;
        for (int i = 0; i <N; i++){ //is 2 right?
          for (int j=0; j<N; j++){
            dot_d1 = dot_d1 + (A.m0.m[i*N+j] * Xp.m[i*N+j]);
            dot_d2 = dot_d2 + (A.m1.m[i*N+j] * Xp.m[i*N+j]);
          }
        }

        //if (p && (d_equal(dot(A.m0, Xp),b.v[0])) && (d_equal(dot(A.m1, Xp),b.v[1]))){
        if (p && (d_equal(dot_d1,b.v[0])) && (d_equal(dot_d2,b.v[1]))){

          //beta approx
          //Q.X = beta_approx(C,Q.X,A,b,theta,beta);

          fixed_point_precision_16_16 stop = (fixed_point_precision_16_16)1;
          fixed_point_precision_16_16 err = (fixed_point_precision_16_16)0;

          int loop = 0;
          while((loop < 20) &&!(err > epsilon) && !(stop <= (fixed_point_precision_16_16)0.25)){
              loop++;

          print_mat(X);

            Matrix L = {N,N,{0,0,0,0}}; //cholesky(Q.X);

            for (int ii = 0; ii < N; ii++) {
              for (int jj = 0; jj < (ii+1); jj++) {
                fixed_point_precision_16_16 s = 0.0;

                for (int kk = 0; kk < jj; kk++) {
                 s = s + L.m[ii*N+kk] * L.m[jj*N+kk];
                }

               if (ii == jj){
                  fixed_point_precision_16_16 to_root = X.m[ii*N+ii] - s;
                  L.m[ii*N+jj] = sqrt_val(to_root);
                } else {
                  L.m[ii*N+jj] = (fixed_point_precision_16_16)1.0 / L.m[jj*N+jj] * (X.m[ii*N+jj] - s);
                }
              }
            }

            //end cholesky
            printf("L\n");
            print_mat(L);

            //Eq_Sol sol = solve_eq(Q.X,A,C,theta);

            Matrix D = {N,N,{0,0,0,0}};

            //set up
            //Matrix U = mat_sub(Q.X,scal_div(mat_mul(mat_mul(Q.X,C),Q.X),theta));
            Matrix U1 = {N,N,{0,0,0,0}};
            for (int v = 0; v < N; ++v) {
               for (int j = 0; j < N; ++j) {
                 for (int k = 0; k < N; ++k) {
                      U1.m[v*(N)+j] += X.m[v*N+k] * C.m[k*N+j];
                  }
               }
            }
            Matrix U = {N,N,{0,0,0,0}};
            for (int vv = 0; vv < N; ++vv) {
                for (int j = 0; j < N; ++j) {
                 for (int k = 0; k < N; ++k) {
                      U.m[vv*(N)+j] += U1.m[vv*N+k] * X.m[k*N+j];
                  }
                }
            }

            for (int ie = 0; ie < N*N; ie++){
              U.m[ie] = X.m[ie] - (U.m[ie] / theta);
            }

           fixed_point_precision_16_16 K[10] = {0,0,0,0,0,0,0,0,0,0};
           int Krows = 1;
           int Kcols = N*N;

           for (int id=0; id<N*N; id++){
              K[id] = U.m[id];
            }
            Kcols += M;



            Big_Matrix QQ = {N*N,M,{0,0,0,0,0,0,0,0,0,0}};
            Matrix nX = {N,N,{0,0,0,0}};//scal_mul(Q.X,-1);
            for (int ib = 0; ib < N*N; ib++){
                                      nX.m[ib] = X.m[ib] * -1;
            }

            //fixed_point_precision_16_16 AQm0[9] = {0,0,0,0};
            //fixed_point_precision_16_16 AQm1[9] = {0,0,0,0};
            int AQrows = N;
            int AQcols = N;


            //for(int a = 0; a < A.len; a++){
            //AQ.m0 = scal_div(mat_mul(mat_mul(nX,AQ.m0),Q.X),theta);
            Matrix AT0 = {N,N,{0,0,0,0}};
            Matrix AT1 = {N,N,{0,0,0,0}};
            for (int vb = 0; vb < N; ++vb) {
                                for (int jb = 0; jb < N; ++jb) {
                                  for (int kb = 0; kb < N; ++kb) {
                                                      AT0.m[vb*(N)+jb] += get(vb,kb,nX) * get(kb,jb,A.m0);
                                                      AT1.m[vb*(N)+jb] += get(vb,kb,nX) * get(kb,jb,A.m1);
                                  }
                                }
            }

            Matrix AT00 = {N,N,{0,0,0,0}};
            Matrix AT11 = {N,N,{0,0,0,0}};
            for (int va = 0; va < N; ++va) {
                                for (int ja = 0; ja < N; ++ja) {
                                  for (int ka = 0; ka < N; ++ka) {
                                                      AT00.m[va*(N)+ja] += get(va,ka,AT0) * get(ka,ja,X);
                                                      AT11.m[va*(N)+ja] += get(va,ka,AT1) * get(ka,ja,X);
                                  }
                                }
            }

            for (int ia = 0; ia < N*N; ia++){
                                      AT00.m[ia] = AT00.m[ia] / theta;
                                      AT11.m[ia] = AT11.m[ia] / theta;
            }
            //AQ.m0 = flatten(AQ.m0);
            AQcols = N*N;
            AQrows = 1;

            //AQ.m1 = scal_div(mat_mul(mat_mul(nX,AQ.m1),Q.X),theta);
            //AQ.m1 = flatten(AQ.m1);

            //}
            for (int c = 0; c < N*N; c++){
                                      //for (int b = 0; b < Q.cols; b++){
                                      QQ.m[c*(M)+0] = get(0,c,AT00);
                                      QQ.m[c*(M)+1] = get(0,c,AT11);
                                      //}
            }


            Big_Matrix R = {M,N*N,{0,0,0,0,0,0,0,0,0,0}};

            //for (int d = 0; d < R.rows; d++){
            for (int ee = 0; ee < N*N; ee++){
                                      R.m[0*(N*N)+ee] = A.m0.m[ee];
                                      R.m[1*(N*N)+ee] = A.m1.m[ee];
            }


            Big_Matrix LL = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
        //prepend identity matrix to Q
        for (int fq = 0; fq < N*N; fq++){
          for (int gq = 0; gq < N*N; gq++){
            if (fq == gq){
              LL.m[fq*(N*N+M)+gq] = 1.0;
            }
        }
        for (int gg = N*N; gg < (N*N + M); gg++){
          LL.m[fq*(N*N + M)+gg] = get_big(fq,(gg-N*N),QQ,M);
        }
      }
      // append P, zeros
      for (int ip = (N*N); ip < (N*N + M); ip++){
        for (int jp = 0; jp < (N*N); jp++){
          LL.m[ip*(N*N + M)+jp] = get_big((ip-N*N),jp,R,N*N);
        }
        for (int hp = (N*N); hp < (N*N + M); hp++){
          LL.m[ip*(N*N + M)+hp] = 0.0;
        }
      }


      //least sq solution
      //Big_Vector e = vectorize(K);
      Big_Vector e = {Krows*Kcols,{0,0,0,0,0,0,0,0,0,0}};
      for (int iik = 0; iik<e.len; iik++){
        e.v[iik] = K[iik];
      }

      //Big_Vector z = LUP_solve(LL, e);
       //Big_Matrix_List LU_List = LUP_decompose_big(LL);
       //printf("REAL LU");
       //print_mat_b(LU_List.m0);


      Big_Matrix LD = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
      Big_Matrix UD = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
      Big_Matrix PD = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
      int n = (N*N + M);

      //pivot A P
      for (int ld=0; ld<n;ld++){
        for (int gd=0; gd<n; gd++){
          if (ld == gd){
            PD.m[ld*(N*N + M)+gd] = 1.0;
          }
        }
      }

      for (int yd=0; yd<n; yd++){
        int max_j = yd;
        for (int rd = yd; rd < n; rd++){
          if (abs_val(get_big(rd,yd,LL,(N*N + M))) > abs_val(get_big(max_j,yd,LL,(N*N + M)))){
            max_j = rd;
          }
        }

        if (max_j != yd){
          //PD = swap_rows_big(PD,yd,max_j);
          for (int sr = 0; sr < (N*N + M); sr++){
            fixed_point_precision_16_16 temp = get_big(yd, sr, PD, (N*N + M));
            PD.m[yd*(N*N + M)+sr] = get_big(max_j, sr, PD, (N*N + M));
            PD.m[max_j*(N*N + M)+sr] = temp;
          }
        }
      }



      //Big_Matrix LLp = mat_mul_big(PD,LL);
      Big_Matrix LLp = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};

      for (int vll = 0; vll < (N*N + M); ++vll) {
          for (int jll = 0; jll < (N*N + M); ++jll) {
            for (int kll = 0; kll < (N*N + M); ++kll) {
                LLp.m[vll*(N*N + M)+jll] += get_big(vll,kll,PD, (N*N + M)) * get_big(kll,jll,LL,(N*N + M));
            }
          }
      }

      //printf("MY AP");
      //print_mat_b(LLp);

      for (int vd=0; vd<n; vd++){
        LD.m[vd*(N*N + M)+vd] = 1.0;
      }

      //printf("MY PRE L");
      //print_mat_b(LD);



      for (int ig=0;ig<n;ig++){

        for (int je=0;je<n;je++){
          fixed_point_precision_16_16 se;
          if (je <= ig){
            se = 0;
            for (int ce = 0; ce < je; ce++){
              se += get_big(je,ce,LD,(N*N + M)) * get_big(ce,ig,UD,(N*N + M));
            }
            UD.m[je*(N*N + M)+ig] = (get_big(je,ig,LLp,(N*N + M)) - se);
          }
          if (je >= ig){
            se = 0;
            for (int ke = 0; ke < ig; ke++){
              se += get_big(je,ke,LD,(N*N + M)) * get_big(ke,ig,UD,(N*N + M));
            }
            LD.m[je*(N*N + M)+ig] = (get_big(je,ig,LLp,(N*N + M)) - se) / get_big(ig,ig,UD,(N*N + M));
          }
        }
      }


      Big_Matrix I = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};

      for (int t = 0; t < (N*N + M); t++){
        for (int u = 0; u < (N*N + M); u++){
          if (t == u){
            I.m[t*(N*N + M)+u] = 1.0;
          }
        }
      }

      //Big_Matrix_List LU_List = {2, , PD};


      // end decomp

      //printf("LD");
      //print_vec(LD);

      //Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
      Big_Matrix LU = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
      for (int tu = 0; tu < (N*N + M); tu++){
        for (int uu = 0; uu < (N*N + M); uu++){
          LU.m[tu*(N*N + M)+uu] = get_big(tu,uu,LD,(N*N + M)) + get_big(tu,uu,UD,(N*N + M)) - get_big(tu,uu,I,(N*N + M));
        }
        }



        //Big_Vector z = vec_mul_big(PD,e);
      Big_Vector z = {(N*N + M),{0,0,0,0,0,0,0,0,0,0}};

      for (int iz = 0; iz < (N*N + M); ++iz) {
        for (int kz = 0; kz < (N*N + M); ++kz) {
            z.v[iz] += get_big(iz,kz,PD,(N*N + M)) * e.v[kz];
        }
      }

      // forward substitute
      for (int a = 0; a < n-1; a++){
        for (int b = a+1; b < n; b++){
          z.v[b] = z.v[b] - (get_big(b,a,LU,(N*N + M)) * z.v[a]);
        }
      }

      // backwards substitute
      for (int i = (n-1); i >= 0; i--){
        if (abs_val(get_big(i,i,LU,(N*N + M))) < epsilon){
          valid = 0;
        }
        z.v[i] = z.v[i] / get_big(i,i,LU,(N*N + M));
        for (int j = 0; j < i; j++){
          z.v[j] = z.v[j] - (get_big(j,i,LU,(N*N + M)) * z.v[i]);
        }
      }

      //end LUP solve


      //norm - for res error;
      //Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
      //fixed_point_precision_16_16 err = norm_vec_big(res);
      fixed_point_precision_16_16 err = norm_circ(e, LL, (N*N + M), z);

      for (int l = 0; l < N*N; l++){
        D.m[l] = z.v[l];
      }

      //D = scal_mul(mat_add(D,transpose(D)),0.5);
      Matrix T = D;
      for (int it = 0; it < N; it++) {
        for (int jt = 0; jt < N; jt++) {
            T.m[it*(N)+jt] = get(jt,it,D);
        }
      }
      for (int kt = 0; kt < (N*N); kt++){
        D.m[kt] = (D.m[kt] + T.m[kt]) * (fixed_point_precision_16_16)0.5;
      }


      for (int o = N*N; o < (N*N)+M; o++){
        printf("z = %6.3f\n", z.v[o]);
        new_y.v[o-(N*N)] = z.v[o];
      }

      // end solve_eq
      printf("Y");
      print_vec(new_y);

      if (err <= epsilon){

        //Q.y = new_y;

        //Matrix S = mat_sub(C, mat_comb(y,A));

        //Matrix I = mat_mul(mat_mul(inverse(L),D),inverse(transpose(L)));
        //stop = norm_mat(I);
        stop = norm_mat_circ(L, N, D);

        if (stop > (fixed_point_precision_16_16)0.25){
          fixed_point_precision_16_16 alpha = (fixed_point_precision_16_16)0.2 / stop;

          Matrix XaD = {N,N,{0,0,0,0}};//mat_add(Q.X,scal_mul(D,alpha));
          for (int xx = 0; xx < (N*N); xx++){
            XaD.m[xx] = X.m[xx] + (D.m[xx] * alpha);
          }
          while (!psd(XaD)){
            alpha = alpha * (fixed_point_precision_16_16)0.5;
          }

          //Q.X = mat_add(Q.X,scal_mul(D,alpha));
          for (int xxx = 0; xxx < (N*N); xxx++){
            X.m[xxx] = X.m[xxx] + (D.m[xxx] * alpha);
          }

        }
      }


    } // end while\n
          printf("loop %d\n", loop);
          print_mat(X);
          printf("beta done\n");
                      //solving - real SDP
          fixed_point_precision_16_16 errs = (fixed_point_precision_16_16)0;
          int loop2 = 0;
          while((loop2 < 20) && !(errs > (fixed_point_precision_16_16)1e-2) && !(theta < (fixed_point_precision_16_16)1e-4)){
            loop2++;

                              //2. shrink T (Theta);
            fixed_point_precision_16_16 alpha = (fixed_point_precision_16_16)0.8; //1 - ((sqrt(beta) - beta)/(sqrt(b)+sqrt(n))); //alpha in (0, 1)

            theta = alpha*theta;

            //3. compute newton direction and multipliers
            // factor Xb = L * Lt
            // solve system of equations

            //Eq_Sol sols = solve_eq(Q.X,A,C,theta);

            Matrix D0 = {N,N,{0,0,0,0}};

            //set up
            //Matrix U = mat_sub(Q.X,scal_div(mat_mul(mat_mul(Q.X,C),Q.X),theta));
            Matrix U10 = {N,N,{0,0,0,0}};
            for (int v0 = 0; v0 < N; ++v0) {
                                for (int j0 = 0; j0 < N; ++j0) {
                                  for (int k0 = 0; k0 < N; ++k0) {
                                                      U10.m[v0*(N)+j0] += get(v0,k0,X) * get(k0,j0,C);
                                  }
                                }
            }

            Matrix U0 = {N,N,{0,0,0,0}};
            for (int vv0 = 0; vv0 < N; ++vv0) {
                                for (int jj0 = 0; jj0 < N; ++jj0) {
                                  for (int kk0 = 0; kk0 < N; ++kk0) {
                                                      U0.m[vv0*(N)+jj0] += get(vv0,kk0,U10) * get(kk0,jj0,X);
                                  }
                                }
            }

            for (int ie0 = 0; ie0 < N*N; ie0++){
                                      U0.m[ie0] = X.m[ie0] - (U0.m[ie0] / theta);
            }

           fixed_point_precision_16_16 K0[10] = {0,0,0,0,0,0,0,0,0,0};
           int Krows0 = 1;
           int Kcols0 = N*N;

           for (int id0=0; id0<N*N; id0++){
                                      K0[id0] = U0.m[id0];
            }
            Kcols0 += M;


            Big_Matrix QQ0 = {N*N,M,{0,0,0,0,0,0,0,0,0,0}};
            Matrix nX0 = {N,N,{0,0,0,0}};//scal_mul(Q.X,-1);
            for (int ib0 = 0; ib0 < N*N; ib0++){
                                      nX0.m[ib0] = X.m[ib0] * -1;
            }

            int AQrowsz = N;
            int AQcolsz = N;

            //for(int a = 0; a < A.len; a++){
            //AQ.m0 = scal_div(mat_mul(mat_mul(nX,AQ.m0),Q.X),theta);
            Matrix AT0z = {N,N,{0,0,0,0}};
            Matrix AT1z = {N,N,{0,0,0,0}};
            for (int vb0 = 0; vb0 < N; ++vb0) {
                                for (int jb0 = 0; jb0 < N; ++jb0) {
                                  for (int kb0 = 0; kb0 < N; ++kb0) {
                                                      AT0z.m[vb0*(N)+jb0] += get(vb0,kb0,nX0) * get(kb0,jb0,A.m0);
                                                      AT1z.m[vb0*(N)+jb0] += get(vb0,kb0,nX0) * get(kb0,jb0,A.m1);;
                                  }
                                }
            }

            Matrix AT00z = {N,N,{0,0,0,0}};
            Matrix AT11z = {N,N,{0,0,0,0}};
            for (int va0 = 0; va0 < N; ++va0) {
                                for (int ja0 = 0; ja0 < N; ++ja0) {
                                  for (int ka0 = 0; ka0 < N; ++ka0) {
                                                      AT00z.m[va0*(N)+ja0] += get(va0,ka0,AT0z) * get(ka0,ja0,X);
                                                      AT11z.m[va0*(N)+ja0] += get(va0,ka0,AT1z) * get(ka0,ja0,X);
                                  }
                                }
            }

            for (int ia0 = 0; ia0 < N*N; ia0++){
                                      AT00z.m[ia0] = AT00z.m[ia0] / theta;
                                      AT11z.m[ia0] = AT11z.m[ia0] / theta;
            }
            //AQ.m0 = flatten(AQ.m0);
            AQcolsz = N*N;
            AQrowsz = 1;

            //AQ.m1 = scal_div(mat_mul(mat_mul(nX,AQ.m1),Q.X),theta);
            //AQ.m1 = flatten(AQ.m1);

            //}
            for (int c0 = 0; c0 < N*N; c0++){
                                      //for (int b = 0; b < Q.cols; b++){
                                      QQ0.m[c0*(M)+0] = get(0,c0,AT00z);
                                      QQ0.m[c0*(M)+1] = get(0,c0,AT11z);
                                      //}
            }

            Big_Matrix R0 = {M,N*N,{0,0,0,0,0,0,0,0,0,0}};
            //for (int d = 0; d < R.rows; d++){
            for (int ee0 = 0; ee0 < N*N; ee0++){
                                      R0.m[0*(N*N)+ee0] = A.m0.m[ee0];
                                      R0.m[1*(N*N)+ee0] = A.m1.m[ee0];
            }

            Big_Matrix LL0 = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
            //prepend identity matrix to Q
            for (int fq0 = 0; fq0 < N*N; fq0++){
              for (int gq0 = 0; gq0 < N*N; gq0++){
                if (fq0 == gq0){
                  LL0.m[fq0*(N*N + M)+gq0] = 1.0;
                }
            }
            for (int gg0 = N*N; gg0 < (N*N + M); gg0++){
              LL0.m[fq0*(N*N + M)+gg0] = get_big(fq0,(gg0-N*N),QQ0,M);
            }
          }
          // append P, zeros
          for (int ip0 = N*N; ip0 < (N*N + M); ip0++){
            for (int jp0 = 0; jp0 < N*N; jp0++){
              LL0.m[ip0*(N*N + M)+jp0] = get_big((ip0-N*N),jp0,R0,N*N);
            }
            for (int hp0 = N*N; hp0 < (N*N + M); hp0++){
              LL0.m[ip0*(N*N + M)+hp0] = 0.0;
            }
          }

          //least sq solution
          //Big_Vector e = vectorize(K);
          Big_Vector e0 = {Krows0*Kcols0,{0,0,0,0,0,0,0,0,0,0}};
          for (int iik0 = 0; iik0<e0.len; iik0++){
            e0.v[iik0] = K0[iik0];
          }

          //Big_Vector z = LUP_solve(LL, e);
          //Big_Matrix_List LU_List = LUP_decompose_big(LL);
                            Big_Matrix LD0 = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
                            Big_Matrix UD0 = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
                            Big_Matrix PD0 = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
                            int n0 = (N*N + M);

                            //pivot A P
                            for (int ld0=0; ld0<n0;ld0++){
                              for (int gd0=0; gd0<n0; gd0++){
                                if (ld0 == gd0){
                                  PD0.m[ld0*(N*N + M)+gd0] = 1.0;
                                }
                              }
                            }

                                              for (int yd0=0; yd0<n0; yd0++){
                              int max_j0 = yd0;
                              for (int rd0 = yd0; rd0 < n0; rd0++){
                                if (abs_val(get_big(rd0,yd0,LL0,(N*N + M))) > abs_val(get_big(max_j0,yd0,LL0,(N*N + M)))){
                                  max_j0 = rd0;
                                }
                              }

                              if (max_j0 != yd0){
                                //PD = swap_rows_big(PD,yd,max_j);
                                for (int sr0 = 0; sr0 < (N*N + M); sr0++){
                                  fixed_point_precision_16_16 temp0 = get_big(yd0, sr0, PD0,(N*N + M));
                                  PD0.m[yd0*(N*N + M)+sr0] = get_big(max_j0, sr0, PD0,(N*N + M));
                                  PD0.m[max_j0*(N*N + M)+sr0] = temp0;
                                }
                              }
                            }

                                              //Big_Matrix LLp = mat_mul_big(PD,LL);
                            Big_Matrix LLp0 = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};

                            for (int vll0 = 0; vll0 < (N*N + M); ++vll0) {
                                for (int jll0 = 0; jll0 < (N*N + M); ++jll0) {
                                  for (int kll0 = 0; kll0 < (N*N + M); ++kll0) {
                                      LLp0.m[vll0*(N*N + M)+jll0] += get_big(vll0,kll0,PD0,(N*N + M)) * get_big(kll0,jll0,LL0,(N*N + M));
                                  }
                                }
                            }

                                              for (int vd0=0; vd0<n0; vd0++){
                                                      LD0.m[vd0*(N*N + M)+vd0] = 1.0;
                                              }

                                              for (int ig0=0;ig0<n0;ig0++){

                                                      for (int je0=0;je0<n0;je0++){
                                                              fixed_point_precision_16_16 se0;
                                                              if (je0 <= ig0){
                                                                      se0 = 0;
                                                                      for (int ce0 = 0; ce0 < je0; ce0++){
                                                                              se0 += get_big(je0,ce0,LD0,(N*N + M)) * get_big(ce0,ig0,UD0,(N*N + M));
                                                                      }
                                                                      UD0.m[je0*(N*N + M)+ig0] = (get_big(je0,ig0,LLp0,(N*N + M)) - se0);
                                                              }
                                                              if (je0 >= ig0){
                                                                      se0 = 0;
                                                                      for (int ke0 = 0; ke0 < ig0; ke0++){
                                                                              se0 += get_big(je0,ke0,LD0,(N*N + M)) * get_big(ke0,ig0,UD0,(N*N + M));
                                                                      }
                                                                      LD0.m[je0*(N*N + M)+ig0] = (get_big(je0,ig0,LLp0,(N*N + M)) - se0) / get_big(ig0,ig0,UD0,(N*N + M));
                                                              }
                                                      }
                                              }

                                              Big_Matrix I0 = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};

                            for (int t0 = 0; t0 < (N*N + M); t0++){
                              for (int u0 = 0; u0 < (N*N + M); u0++){
                                if (t0 == u0){
                                  I0.m[t0*(N*N + M)+u0] = 1.0;
                                }
                              }
                            }

                            //Big_Matrix_List LU_List = {2, , PD};


                            // end decomp

                                              //Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
                                              Big_Matrix LU0 = {(N*N + M),(N*N + M),{0,0,0,0,0,0,0,0,0,0}};
                                              for (int tu0 = 0; tu0 < (N*N + M); tu0++){
                              for (int uu0 = 0; uu0 < (N*N + M); uu0++){
                                LU0.m[tu0*(N*N + M)+uu0] = get_big(tu0,uu0,LD0,(N*N + M)) + get_big(tu0,uu0,UD0,(N*N + M)) - get_big(tu0,uu0,I0,(N*N + M));
                              }
                             }

                                               //Big_Vector z = vec_mul_big(PD,e);
                            Big_Vector z0 = {(N*N + M),{0,0,0,0,0,0,0,0,0,0}};

                            for (int iz0 = 0; iz0 < (N*N + M); ++iz0) {
                              for (int kz0 = 0; kz0 < (N*N + M); ++kz0) {
                                  z0.v[iz0] += get_big(iz0,kz0,PD0,(N*N + M)) * e0.v[kz0];
                              }
                            }

                            // forward substitute
                            for (int a0 = 0; a0 < n0-1; a0++){
                              for (int b0 = a0+1; b0 < n0; b0++){
                                z0.v[b0] = z0.v[b0] - (get_big(b0,a0,LU0,(N*N + M)) * z0.v[a0]);
                              }
                            }

                            // backwards substitute
                            for (int i0 = (n0-1); i0 >= 0; i0--){
                              if (abs_val(get_big(i0,i0,LU0,(N*N + M))) < epsilon){
                                valid = 0;
                              }
                              z0.v[i0] = z0.v[i0] / get_big(i0,i0,LU0,(N*N + M));
                              for (int j0 = 0; j0 < i0; j0++){
                                z0.v[j0] = z0.v[j0] - (get_big(j0,i0,LU0,(N*N + M)) * z0.v[i0]);
                              }
                            }

                            //end LUP solve

                                              //norm - for res error;
                                              //Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
                                              //fixed_point_precision_16_16 err = norm_vec_big(res);
                                              fixed_point_precision_16_16 errs = norm_circ(e0, LL0, (N*N + M), z0);

                                              for (int l0 = 0; l0 < N*N; l0++){
                              D0.m[l0] = z0.v[l0];
                            }

                            //D = scal_mul(mat_add(D,transpose(D)),0.5);
                            Matrix T0 = D0;
                            for (int it0 = 0; it0 < N; it0++) {
                              for (int jt0 = 0; jt0 < N; jt0++) {
                                  T0.m[it0*(N)+jt0] = get(jt0,it0,D0);
                              }
                            }
                            for (int kt0 = 0; kt0 < N*N; kt0++){
                              D0.m[kt0] = (D0.m[kt0] + T0.m[kt0]) * (fixed_point_precision_16_16)0.5;
                            }


                            for (int o0 = N*N; o0 < (N*N)+M; o0++){
                              new_y.v[o0-(N*N)] = z0.v[o0];
                            }

                                              // end solve_eq


            if (errs <= epsilon){

              //4. update all values
              //Q.y = new_y;



              fixed_point_precision_16_16 t = 1.0;

              //Matrix XDT = mat_add(Q.X,scal_mul(sols.D,t));
                                      Matrix XDT = {N,N,{0,0,0,0}};
                                      for (int xy = 0; xy < N*N; xy++){
                                              XDT.m[xy] = X.m[xy] + (D0.m[xy] * t);
                                      }
// start psd(XDT)
int psd_XDT = 1;

        //fixed_point_precision_16_16 R[4] = {0,0,0,0};
        fixed_point_precision_16_16 r00w = 0.0;
        fixed_point_precision_16_16 r01w = 0.0;
        fixed_point_precision_16_16 r10w = 0.0;
        fixed_point_precision_16_16 r11w = 0.0;

        //i = 0
          //j=0
        fixed_point_precision_16_16 sw = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 to_rootw = XDT.m[0] - sw;
        if (to_rootw < (fixed_point_precision_16_16)(0.0)){
          psd_XDT = 0;
        } else {
          fixed_point_precision_16_16 rootw = sqrt_val(to_rootw);
          r00w = rootw;
        }


        //i = 1
          //j=0
        sw = (fixed_point_precision_16_16)0.0;
        r10w = (fixed_point_precision_16_16)(1.0) / r00w * (XDT.m[2] - sw);


          //j=1
        sw = r10w * r10w;
        to_rootw = XDT.m[3] - sw;
        if (to_rootw < (fixed_point_precision_16_16)(0.0)){
                psd_XDT = 0;
         } else {
                fixed_point_precision_16_16 rootw = sqrt_val(to_rootw);
                r11w = rootw;
         }

// end psd

              while (!psd_XDT){
                t = alpha * t;
                //XDT = mat_add(Q.X,scal_mul(sols.D,t));
                                              for (int xz = 0; xz < N*N; xz++){
                                                      XDT.m[xz] = X.m[xz] + (D0.m[xz] * t);
                                              }
              }

              if (theta >= (fixed_point_precision_16_16)1e-4){
                X.m[0] = XDT.m[0];
                X.m[1] = XDT.m[1];
                X.m[2] = XDT.m[2];
                X.m[3] = XDT.m[3];
              }

// start psd(XDT)
psd_XDT = 1;

        //fixed_point_precision_16_16 R[4] = {0,0,0,0};
        fixed_point_precision_16_16 r00 = 0.0;
        fixed_point_precision_16_16 r01 = 0.0;
        fixed_point_precision_16_16 r10 = 0.0;
        fixed_point_precision_16_16 r11 = 0.0;

        //i = 0
          //j=0
        fixed_point_precision_16_16 s = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 to_root = XDT.m[0] - s;
        if (to_root < (fixed_point_precision_16_16)(0.0)){
          psd_XDT = 0;
        } else {
          fixed_point_precision_16_16 root = sqrt_val(to_root);
          r00 = root;
        }


        //i = 1
          //j=0
        s = (fixed_point_precision_16_16)0.0;
        r10 = (fixed_point_precision_16_16)(1.0) / r00 * (XDT.m[2] - s);


          //j=1
        s = r10 * r10;
        to_root = XDT.m[3] - s;
        if (to_root < (fixed_point_precision_16_16)(0.0)){
                psd_XDT = 0;
         } else {
                fixed_point_precision_16_16 root = sqrt_val(to_root);
                r11 = root;
         }

// end psd



            }


                      }


              } // end if


      Problem_Box P = {A.m0.m[0],A.m0.m[1],A.m0.m[2],A.m0.m[3],
                       A.m1.m[0],A.m1.m[1],A.m1.m[2],A.m1.m[3],
                       C.m[0],C.m[1],C.m[2],C.m[3],
                       X.m[0],X.m[1],X.m[2],X.m[3],b.v[0],b.v[1],new_y.v[0],new_y.v[1],1};

        return P;
      }




int sdp_check(Problem_Box P, int feasible){

int solved = 1;

if (feasible){

  // (X) feasible
  //solved = solved && psd(X);


        int n = N;
        //fixed_point_precision_16_16 R[4] = {0,0,0,0};
        fixed_point_precision_16_16 r00 = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 r01 = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 r10 = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 r11 = (fixed_point_precision_16_16)0.0;

        //i = 0
          //j=0
        fixed_point_precision_16_16 s = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 to_root = P.X_0 - s;
        if (to_root < (fixed_point_precision_16_16)(0.0)){
          solved = 0;
        } else {
          fixed_point_precision_16_16 root = sqrt_val(to_root);
          r00 = root;
        }


          //j=0
        // = 1
        s = (fixed_point_precision_16_16)0.0;
        r10 = (fixed_point_precision_16_16)(1.0) / r00 * (P.X_2 - s);


          //j=1
        s = r10 * r10;
        to_root = P.X_3 - s;
        if (to_root < (fixed_point_precision_16_16)(0.0)){
                solved = 0;
         } else {
                fixed_point_precision_16_16 root = sqrt_val(to_root);
                r11 = root;
         }










  fixed_point_precision_16_16 dot_s0f = (P.A0_0*P.X_0) + (P.A0_1*P.X_1) + (P.A0_2*P.X_2) + (P.A0_3*P.X_3);
  fixed_point_precision_16_16 dot_s1f = (P.A1_0*P.X_0) + (P.A1_1*P.X_1) + (P.A1_2*P.X_2) + (P.A1_3*P.X_3);

  solved = solved && d_equal(dot_s0f,P.b0);
  solved = solved && d_equal(dot_s0f,P.b1);

  //}

  // (y,S) feasible
  //Matrix S = mat_sub(C, mat_comb(y,A)); // sum from 1 to m of yi*Ai
  //comb - Matrix sum = scal_mul(A.m0, y.v[0]);
  //sum = mat_add(sum,scal_mul(A.m1, y.v[1]));
//    Matrix S = {N,N,{0,0,0,0}};
//    for (int i = 0; i < S.rows*S.cols; i++){
      fixed_point_precision_16_16 S_0 = P.C_0 - ((P.A0_0 * P.y0) + (P.A1_0 * P.y1));
      fixed_point_precision_16_16 S_1 = P.C_1 - ((P.A0_1 * P.y0) + (P.A1_1 * P.y1));
      fixed_point_precision_16_16 S_2 = P.C_2 - ((P.A0_2 * P.y0) + (P.A1_2 * P.y1));
      fixed_point_precision_16_16 S_3 = P.C_3 - ((P.A0_3 * P.y0) + (P.A1_3 * P.y1));

//    }

//    solved = solved && psd(S);

        //fixed_point_precision_16_16 R[4] = {0,0,0,0};
        fixed_point_precision_16_16 r00s = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 r01s = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 r10s = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 r11s = (fixed_point_precision_16_16)0.0;

        //i = 0
          //j=0
        fixed_point_precision_16_16 ss = (fixed_point_precision_16_16)0.0;
        fixed_point_precision_16_16 to_roots = P.X_0 - ss;
        if (to_roots < (fixed_point_precision_16_16)(0.0)){
          solved = 0;
        } else {
          fixed_point_precision_16_16 roots = sqrt_val(to_roots);
          r00s = roots;
        }
          //j=0
        // = 1
        ss = (fixed_point_precision_16_16)0.0;
        r10s = (fixed_point_precision_16_16)(1.0) / r00s * (P.X_2 - ss);


          //j=1
        ss = r10s * r10s;
        to_roots = P.X_3 - ss;
        if (to_roots < (fixed_point_precision_16_16)(0.0)){
                solved = 0;
         } else {
                fixed_point_precision_16_16 roots = sqrt_val(to_roots);
                r11s = roots;
         }







  // C*X - sum(yi*bi) = 0 (duality gap = 0)
  fixed_point_precision_16_16 gap = (S_0*P.X_0) + (S_1*P.X_1) + (S_2*P.X_2) + (S_3*P.X_3); // = dot(S,X);
//dot(P.C,P.X) - vec_comb(P.y,P.b);

  solved = solved && (d_equal_ep(gap,0.0,1e-2));

  //printf("gap %6f\n", gap);

} else { //infeasibly

  solved = 0;
  // X doesn't fit problem
  //for (int f = 0; f<P.A.len; f++){

  fixed_point_precision_16_16 dot_s0 = (P.A0_0*P.X_0) + (P.A0_1*P.X_1) + (P.A0_2*P.X_2) + (P.A0_3*P.X_3);
  fixed_point_precision_16_16 dot_s1 = (P.A1_0*P.X_0) + (P.A1_1*P.X_1) + (P.A1_2*P.X_2) + (P.A1_3*P.X_3);

   if (!(d_equal(dot_s0,P.b0))){
      solved = 1;
   }
    if (!(d_equal(dot_s0,P.b1))){
      solved = 1;
    }
}
return solved;

}
/*
Matrix sdp_gadget(Matrix C, Matrix X, Matrix_List A, Vector b) {

Problem_Box Q = __GADGET_compute(sdp(C,X,A,b,1.0,0.25));

__GADGET_check(sdp_check(Q, Q.feasible));
Matrix X = {N,N,{Q.X_0,Q.X_1,Q.X_2,Q.X_3}};

return X;
}
*/
