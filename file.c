typedef float fp64;

#define epsilon (fp64)1.0e-2

int hw(fp64 x){
  return 1;
}

int d_equal(fp64 a, fp64 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b);
  }
}

fp64 abs_val(fp64 x){
  if (x < (fp64)0.0){
    return x*-1;
  } else {
    return x;
  }
}

fp64 sqrt_val(fp64 num){

  //TODO NEG catch?

  int start = 0;
  int end = num;
  int mid;
  fp64 ans;

  while (start <= end && mid*mid !=num) {

      mid = (start + end) / 2;

      if (mid * mid == num) {
    ans = (fp64)mid;
      }

      if (mid * mid < num) {
    start = mid + 1;
    ans = (fp64)mid;
      }

      else {
    end = mid - 1;
      }
  }

  fp64 increment = (fp64)0.1;
  for (int i = 0; i < 5; i++) {
      while (ans * ans <= num) {
    ans += (fp64)increment;
      }

      ans = ans - (fp64)increment;
      increment = increment / 10;
  }

  return ans;

}

int test_check(fp64 x0){
  int solved = (d_equal(x0,1.706));
  return solved;
}

//n,m, C, X, big array of A's, b, sol_y, sol_x
int check_sdp(int n,int m,fp64 c0, fp64 c1, fp64 c2, fp64 c3, fp64 c4, fp64 c5, fp64 c6, fp64 c7, fp64 c8, fp64 x0, fp64 x1, fp64 x2, fp64 x3, fp64 x4, fp64 x5, fp64 x6, fp64 x7, fp64 x8,
  fp64 a0_0, fp64 a0_1, fp64 a0_2, fp64 a0_3, fp64 a0_4, fp64 a0_5, fp64 a0_6, fp64 a0_7, fp64 a0_8, fp64 a1_0, fp64 a1_1, fp64 a1_2, fp64 a1_3, fp64 a1_4, fp64 a1_5, fp64 a1_6, fp64 a1_7, fp64 a1_8,
  fp64 b0, fp64 b1, fp64 y0, fp64 y1){

  int solved = 1;
	//printf("SAT = %d\n", solved);

  if (1){ // x feasible
    //solved = solved && psd(X);
    fp64 det = (x0*x3) - (x1*x2);

    solved = solved && (x0 > -0.01) && (x3 > -0.01) && (det > -0.01);

    fp64 dot_s0f = (a0_0*x0) + (a0_1*x1) + (a0_2*x2) + (a0_3*x3);
    fp64 dot_s1f = (a1_0*x0) + (a1_1*x1) + (a1_2*x2) + (a1_3*x3);

//    solved = solved && d_equal(dot_s0f,b0);
//    solved = solved && d_equal(dot_s1f,b1);
/*

      // (y,S) feasible
      //Matrix S = mat_sub(C, mat_comb(y,A)); // sum from 1 to m of yi*Ai
    fp64 s0 = c0 - ((a0_0 * y0) + (a1_0 * y1));
    fp64 s1 = c1 - ((a0_1 * y0) + (a1_1 * y1));
    fp64 s2 = c2 - ((a0_2 * y0) + (a1_2 * y1));
    fp64 s3 = c3 - ((a0_3 * y0) + (a1_3 * y1));

    // psd(S);

    fp64 det2 = (s0*s3) - (s1*s2);

    solved = solved && (s0 > -0.01) && (s3 > -0.01) && (det2 > -0.01);

      // C*X - sum(yi*bi) = 0 (duality gap = 0)
    fp64 gap = (s0*x0) + (s1*x1) + (s2*x2) + (s3*x3); // = dot(S,X);


    solved = solved && (d_equal(gap,0.0));
  */

  }

  return solved;


}


int test(){
/*
  fp64 C[9] = {-0.99154214,  0.65386878, -0.64033738, 0.65386878,  0.93792596, -0.14210919, -0.64033738, -0.14210919,  0.30795382};
  double b[9] = {3.9888687923835366, -1.0823059466682765,0,0,0,0,0,0,0};
  double X[9] = {1.78032993, -0.00358676,  0.72814533, -0.00358676,  1.22372938, -0.05358303, 0.72814533, -0.05358303,  1.84385819};

  double A0[9] = {0.35184943, -0.38338125,  0.31362847, -0.38338125,  0.85702573, -0.58909452, 0.31362847, -0.58909452,  0.97137504};
  double A1[9] = {-0.4945009 , -0.62078468,  0.20384815, -0.62078468,  0.21578012, -0.09717294, 0.20384815, -0.09717294, -0.42178768};

  double A[18] = {0.35184943, -0.38338125,  0.31362847, -0.38338125,  0.85702573, -0.58909452, 0.31362847, -0.58909452,  0.97137504, -0.4945009 , -0.62078468,  0.20384815, -0.62078468,  0.21578012, -0.09717294, 0.20384815, -0.09717294, -0.42178768};

 prob 2x2x2
  Matrix C = {N,N,{-0.1983367,  0.54620727, 0.54620727,  0.29183634}};
  Vector b = {M,{-2.3830572764539832, 0.8521208961278653, 0, 0}};
  Matrix X = {N,N,{1.3713053, 0.16070848,0.16070848, 1.43693619}};

  Matrix A0 = {N,N,{-0.99890972, 0.14410886,0.14410886, -0.73737868}};
  Matrix A1 = {N,N,{0.14047667, -0.17714865,-0.17714865,  0.49857682}};
  Matrix_List A = {M,A0,A1};


*/


  fp64 x0 = 3;// = __GADGET_exist();
  fp64 x1 = 3;// __GADGET_exist();
  fp64 x2 = 3;// __GADGET_exist();
  fp64 x3 = 3;// __GADGET_exist();
  /*
  fp64 x4 = __GADGET_exist();
  fp64 x5 = __GADGET_exist();
  fp64 x6 = __GADGET_exist();
  fp64 x7 = __GADGET_exist();
  fp64 x8 = __GADGET_exist();
  */

  fp64 y0 = 3;//= __GADGET_exist();
  fp64 y1 = 3;//= __GADGET_exist();

//n,m, C, X, big array of A's, b, sol_y, sol_x
  __GADGET_sdp(2,2,-0.1983367,  0.54620727, 0.54620727,  0.29183634,1.3713053, 0.16070848,0.16070848, 1.43693619,-0.99890972, 0.14410886,0.14410886, -0.73737868,0.14047667, -0.17714865,-0.17714865,  0.49857682,-2.3830572764539832, 0.8521208961278653);

  int check = __GADGET_check(check_sdp(3,2,-0.1983367,  0.54620727, 0.54620727,  0.29183634, 0,0,0,0,0, x0,x1,x2,x3,0,0,0,0,0,-0.99890972, 0.14410886,0.14410886, -0.73737868,0,0,0,0,0, 0.14047667, -0.17714865,-0.17714865,  0.49857682,0,0,0,0,0, -2.3830572764539832, 0.8521208961278653, y0,y1));

  //return check;
      return 1;
}
