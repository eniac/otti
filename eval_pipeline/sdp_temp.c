#include <stdio.h>

typedef float fixed_point_precision_16_16;

static const fixed_point_precision_16_16 epsilon   = (fixed_point_precision_16_16)1.0e-5;
static int valid = 1;

$macros

typedef struct {
  $M_vector_elems
} Vector;

typedef struct {
  int len;
  $B_vector_elems
} Big_Vector;

typedef struct {
  $N_matrix_elems
} Matrix;

typedef struct {
  int rows;
  int cols;
  $B_matrix_elems
} Big_Matrix;

typedef struct {
  $M_matrix_list
} Matrix_List;

typedef struct {
  $M_big_matrix_list
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

  $print_mat

  printf("\n----------------\n");

}


void print_mat_b(Big_Matrix X) {

  printf("\n-----------------\n");

  $print_b_mat

  printf("\n----------------\n");
}

void print_vec(Vector v) {

  printf("\n-----------------\n");


  $print_vec

  printf("\n----------------\n");
}

void print_vec_b(Big_Vector v) {

  printf("\n-----------------\n");

  $print_b_vec

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


  Solution Q = sdp(C,X,A,b,(fixed_point_precision_16_16)1.0,(fixed_point_precision_16_16)0.25);
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
$dot
  return s;
}

fixed_point_precision_16_16 vec_comb(Vector a, Vector b){
  fixed_point_precision_16_16 s = 0;
$vec_comb

  return s;
}

Matrix scal_div(Matrix A, fixed_point_precision_16_16 s){
$scal_div
  return A;
}

Matrix mat_mul(Matrix A, Matrix B){
Matrix C = $m_init
$mat_mul
  return C;
}

Big_Matrix mat_mul_big(Big_Matrix A, Big_Matrix B){
Big_Matrix C = {A.rows,A.cols, $b_m_init
$b_mat_mul
  return C;
}

//TODO CHECK
Vector vec_mul(Matrix A, Vector b){
  Vector c = $v_init
$vec_mul
  return c;
}

//TODO CHECK
Big_Vector vec_mul_big(Big_Matrix A, Big_Vector b){
Big_Vector c = {b.len, $b_v_init
$b_vec_mul
  return c;
}

fixed_point_precision_16_16 vec_vec_mul(Vector a, Vector b){
  fixed_point_precision_16_16 c = 0.0;
$vec_vec_mul
  return c;

}

Matrix scal_mul(Matrix A, fixed_point_precision_16_16 s){
$scal_mul
  return A;
}

Matrix mat_add(Matrix A, Matrix B){
  Matrix C = A;
$mat_add
  return C;
}


Big_Matrix mat_add_big(Big_Matrix A, Big_Matrix B){
  Big_Matrix C = A;
$b_mat_add
  return C;
}

Matrix mat_sub(Matrix A, Matrix B){
  Matrix C = A;
$mat_sub
  return C;
}

Big_Matrix mat_sub_big(Big_Matrix A, Big_Matrix B){
  Big_Matrix C = A;
$b_mat_sub
  return C;
}

Matrix mat_comb(Vector y, Matrix_List A){
  Matrix sum = scal_mul(A.m0, y.m0);
$mat_comb
  return sum;

}

Matrix transpose(Matrix A){
  Matrix T = A;
$transpose
  return T;
}

Vector vec_sub(Vector a, Vector b){
  Vector x = a;
$vec_sub
  return x;
}

Big_Vector vec_sub_big(Big_Vector a, Big_Vector b){
  Big_Vector x = a;
$b_vec_sub
  return x;
}

Big_Vector vectorize(Big_Matrix A){
Big_Vector v = {A.rows*A.cols, $b_v_init
$vectorize
  return v;
}

Big_Matrix biggify_mat(Matrix P, int rows, int cols){
Big_Matrix A = {(N*N+M),(N*N+M), $b_m_init
$biggify_mat
  return A;

}


fixed_point_precision_16_16 norm_circ(Big_Vector e, Big_Matrix LL, Big_Vector z){
  //Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
  Big_Vector res = {e.len, $b_v_init

  $nc1

  printf("pre norm vec big\n");
  fixed_point_precision_16_16 err = norm_vec_big(res);
  return err;

}

fixed_point_precision_16_16 norm_vec_big(Big_Vector v){

  fixed_point_precision_16_16 sum = 0.0;
$norm_vec_big

  printf("post pows\n");

  fixed_point_precision_16_16 r = sqrt_val(sum);

  printf("post sq\n");
  return r;
}

fixed_point_precision_16_16 norm_mat_circ(Matrix L, Matrix D){ //L,D regular sized
//Matrix I = mat_mul(mat_mul(inverse(L),D),inverse(transpose(L)));

//LI = inverse(L)
//Matrix_List LU_List = LUP_decompose(L);
Matrix LA = $m_init
Matrix UA = $m_init
Matrix PA = $m_init


//pivot A P
$nmc1

$swap1

//Big_Matrix LLp = mat_mul_big(PD,LL);
Matrix LLp = $m_init

$nmc2

$nmc3

$nmc4

Matrix IDT = $m_init
$nmc5

//Big_Matrix_List LU_List = {2, , PD};

//Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
Matrix LU = $m_init
$nmc6

// end decomp

Matrix P = PA;
Matrix LI = LU;

$nmc7

//LT = transpose(L)
Matrix LT = L;
$nmc8

//LTI = inverse(L.T)
//Matrix_List LU_List = LUP_decompose(LT);
Matrix LB = $m_init
Matrix UB = $m_init
Matrix PB = $m_init

//pivot A P
$nmc9

$swap2

//Big_Matrix LLp = mat_mul_big(PD,LL);
Matrix LLpp = $m_init

$y1

$y2

$y3


//Big_Matrix_List LU_List = {2, , PD};

//Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
Matrix LU2 = $m_init
$y4
// end decomp

Matrix LTI = LU2;

$y5
// end inverse

Matrix LID = $m_init

//I = mul: (L.I * D) * L.T.I
$y6

Matrix I = $m_init

$y7

printf("I EQ");
print_mat(I);

fixed_point_precision_16_16 stop = norm_mat(I);

return stop;
}


fixed_point_precision_16_16 norm_mat(Matrix X){
  fixed_point_precision_16_16 sum = 0.0;
$norm_mat

  fixed_point_precision_16_16 r = sqrt_val(sum);
  return r;
}



int psd(Matrix X){ // X is psd <-> Cholesky decomposable

int r = 1;

Matrix R = $m_init

$psd
//print_mat(R);
return r;
}




Solution sdp(Matrix C, Matrix Xp, Matrix_List A, Vector b, fixed_point_precision_16_16 theta, fixed_point_precision_16_16 beta){


  // Reference: http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-251j-introduction-to-mathematical-programming-fall-2009/readings/MIT6_251JF09_SDP.pdf

  int p = psd(Xp);
  Solution Q = {0,Xp,$v_init_ne
	Vector new_y = $v_init

  if (p $s01){

    //beta approx
    //Q.X = beta_approx(C,Q.X,A,b,theta,beta);

    fixed_point_precision_16_16 stop = 1.0;
    fixed_point_precision_16_16 err = 0.0;

    int loop = 0;
    while((loop < 6) && !(err > epsilon) && !(stop <= (fixed_point_precision_16_16)0.25)){
      loop++;
      printf("beta X");
      print_mat(Q.X);

      Matrix L = $m_init //cholesky(Q.X);

      $s02
      //end cholesky

      printf("L");
      print_mat(L);

      //Eq_Sol sol = solve_eq(Q.X,A,C,theta);

      Matrix D = $m_init

      //set up
      //Matrix U = mat_sub(Q.X,scal_div(mat_mul(mat_mul(Q.X,C),Q.X),theta));
      Matrix U1 = $m_init
      $s03

      Matrix U = $m_init
      $s04

      $s05

      Big_Matrix K = {1, N*N+M, $b_m_init

      $s06


      Big_Matrix QQ = {N*N,M,$b_m_init
      Matrix nX = scal_mul(Q.X,-1);

      printf("nX");
      print_mat(nX);
      /*for (int ib = 0; ib < Q.X.rows*Q.X.cols; ib++){
				nX.m[ib] = Q.X.m[ib] * -1;
      }*/
      //for(int a = 0; a < A.len; a++){
      //Matrix AQ.m0 = scal_div(mat_mul(mat_mul(nX,AQ.m0),Q.X),theta);
      $s07

      printf("AQm...");
      print_mat(AQm0);
      print_mat_b(AQm1);

      /*Matrix AT0 = $m_init
      Matrix AT1 = $m_init
      for (int vb = 0; vb < N; ++vb) {
			  for (int jb = 0; jb < N; ++jb) {
			    for (int kb = 0; kb < N; ++kb) {
						AT0.m[vb*(N)+jb] += get(vb,kb,nX) * get(kb,jb,A.m0);
						AT1.m[vb*(N)+jb] += get(vb,kb,nX) * get(kb,jb,A.m1);
			    }
			  }
      }

      Matrix AT00 = $m_init
      Matrix AT11 = $m_init
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
      $s08



      Big_Matrix R = {M,N*N, $b_m_init
      $s09

      printf("R");
      print_mat_b(R);

      Big_Matrix LL = {(N*N + M),(N*N + M), $b_m_init
        //prepend identity matrix to Q
      $s10

      printf("LL\n");

      // append P, zeros
      $s11


      //least sq solution
      Big_Vector e = vectorize(K);
      $s12

      //Big_Vector z = LUP_solve(LL, e);
       //Big_Matrix_List LU_List = LUP_decompose_big(LL);
       //printf("REAL LU");
       //print_mat_b(LU_List.m0);


      Big_Matrix LD = {(N*N + M),(N*N + M), $b_m_init
      Big_Matrix UD = {(N*N + M),(N*N + M), $b_m_init
      Big_Matrix PD = {(N*N + M),(N*N + M), $b_m_init

      printf("pre piv\n");
      //pivot A P
      $s13

      $swap3

      printf("post piv/swap\n");

      //Big_Matrix LLp = mat_mul_big(PD,LL);
      Big_Matrix LLp = {(N*N + M),(N*N + M), $b_m_init

      $s15

      //printf("MY AP");
      //print_mat_b(LLp);

      $s16

      //printf("MY PRE L");
      //print_mat_b(LD);



      $s17

      Big_Matrix I = {(N*N + M),(N*N + M), $b_m_init

      $s18

      //Big_Matrix_List LU_List = {2, , PD};


      // end decomp

      //Big_Matrix LU = mat_sub_big(mat_add_big(LD,UD),I);
      Big_Matrix LU = {(N*N + M),(N*N + M), $b_m_init
      $s19


        //Big_Vector z = vec_mul_big(PD,e);
      Big_Vector z = {PD.cols, $b_v_init

      $s20

      // forward substitute
      $s21

      // backwards substitute
      $s22

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
      $i1

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


      $s23



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

        if (stop > (fixed_point_precision_16_16)0.25){
          fixed_point_precision_16_16 alpha = (fixed_point_precision_16_16)0.2 / stop;

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
    fixed_point_precision_16_16 errs = (fixed_point_precision_16_16)0.0;
    while(0 && !(errs > (fixed_point_precision_16_16)1e-2) && !(theta < (fixed_point_precision_16_16)1e-4)){

      printf("reg X");
      print_mat(Q.X);

			//2. shrink T (Theta);
      fixed_point_precision_16_16 alpha = (fixed_point_precision_16_16)0.8; //1 - ((sqrt(beta) - beta)/(sqrt(b)+sqrt(n))); //alpha in (0, 1)

      theta = alpha*theta;

      //3. compute newton direction and multipliers
      // factor Xb = L * Lt
      // solve system of equations

      //Eq_Sol sols = solve_eq(Q.X,A,C,theta);

      Matrix D0 = $m_init

      //set up
      //Matrix U = mat_sub(Q.X,scal_div(mat_mul(mat_mul(Q.X,C),Q.X),theta));

      Matrix U10 = $m_init

      $s24

      Matrix U0 = $m_init
      $s25

      $s26

      Big_Vector K0 = {N*N+M,$b_v_init

      $s27


      Big_Matrix QQ0 = {N*N,M,$b_m_init
      Matrix nX0 = $m_init

      $s28


      //for(int a = 0; a < A.len; a++){
      //AQ.m0 = scal_div(mat_mul(mat_mul(nX,AQ.m0),Q.X),theta);
      $s29

      $s30

      $s31

      //AQ.m0 = flatten(AQ.m0);


      //AQ.m1 = scal_div(mat_mul(mat_mul(nX,AQ.m1),Q.X),theta);
      //AQ.m1 = flatten(AQ.m1);

      //}
      $s32

      Big_Matrix R0 = {M,N*N,$b_m_init
      $s33

      Big_Matrix LL0 = {(N*N + M),(N*N + M),$b_m_init
        //prepend identity matrix to Q
      $z13
    // append P, zeros
      $z12

          //least sq solution
          //Big_Vector e = vectorize(K);
          Big_Vector e0 = {10*N*N+M, $b_v_init

          $z11

          //Big_Vector z = LUP_solve(LL, e);
          //Big_Matrix_List LU_List = LUP_decompose_big(LL);
		      Big_Matrix LD0 = {LL0.rows,LL0.cols,$b_m_init
		      Big_Matrix UD0 = {LL0.rows,LL0.cols,$b_m_init
		      Big_Matrix PD0 = {LL0.rows,LL0.cols,$b_m_init
		      int n0 = LL0.rows;

		      //pivot A P
		      $z10




					$swap4

					//Big_Matrix LLp = mat_mul_big(PD,LL);
		      Big_Matrix LLp0 = {PD0.rows,LL0.cols,$b_m_init

		      $z09

					$z08

          $z07

					Big_Matrix I0 = {LL0.rows,LL0.cols,$b_m_init

		      $zi

		      //Big_Matrix_List LU_List = {2, , PD};

		      // end decomp

					Big_Matrix LU0 = {LL0.rows,LL0.cols,$b_m_init
					$z06

					 //Big_Vector z = vec_mul_big(PD,e);
		      Big_Vector z0 = {PD0.cols, $b_v_init

		      $z05

		      // forward substitute
		      $z04

		      // backwards substitute
		      $z03

		      //end LUP solve

					//norm - for res error;
					//Big_Vector res = vec_sub_big(e,vec_mul_big(LL,z));
					//fixed_point_precision_16_16 err = norm_vec_big(res);
					fixed_point_precision_16_16 errs = norm_circ(e0, LL0, z0);

					$z02

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

		      $z01

					// end solve_eq


      if (errs <= 1e-2){

        //4. update all values
        Q.y = new_y;
        printf("Y");
        //print_vec(new_y);



        fixed_point_precision_16_16 t = (fixed_point_precision_16_16)1.0;

        Matrix XDT = mat_add(Q.X,scal_mul(D0,t));
				/*Matrix XDT = $m_init
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
$check2
  //}

  // (y,S) feasible
  //Matrix S = mat_sub(C, mat_comb(y,A)); // sum from 1 to m of yi*Ai
  //comb - Matrix sum = scal_mul(A.m0, y.v[0]);
  //sum = mat_add(sum,scal_mul(A.m1, y.v[1]));
  Matrix S = $m_init
  $check3

  solved = solved && psd(S);

  // C*X - sum(yi*bi) = 0 (duality gap = 0)
  fixed_point_precision_16_16 gap = dot(S,X); //dot(P.C,P.X) - vec_comb(P.y,P.b);
  solved = solved && (d_equal_ep(gap,(fixed_point_precision_16_16)0.0,(fixed_point_precision_16_16)1e-2));

  //printf("gap %6f\n", gap);

} else { //infeasibly

  solved = 0;
  // X doesn't fit problem
  //for (int f = 0; f<P.A.len; f++){
$check1
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
