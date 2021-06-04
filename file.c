#define epsilon (fp32)1.0e-2
typedef double fp32;

int hw(fp32 x){
  return 1;
}

int d_equal(fp32 a, fp32 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b) < epsilon;
  }
}

fp32 abs_val(fp32 x){
  if (x < (fp32)0.0){
    return x*-1;
  } else {
    return x;
  }
}

fp32 sqrt_val(fp32 n){
    fp32 x = n;
    fp32 root;
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

//n,m, C, X, big array of A's, b, sol_y, sol_x

int check_sdp(fp32 n,fp32 m,fp32 c0, fp32 c1, fp32 c2, fp32 c3, fp32 c4, fp32 c5, fp32 c6, fp32 c7, fp32 c8, fp32 x0, fp32 x1, fp32 x2, fp32 x3, fp32 x4, fp32 x5, fp32 x6, fp32 x7, fp32 x8,
  fp32 a0_0, fp32 a0_1, fp32 a0_2, fp32 a0_3, fp32 a0_4, fp32 a0_5, fp32 a0_6, fp32 a0_7, fp32 a0_8, fp32 a1_0, fp32 a1_1, fp32 a1_2, fp32 a1_3, fp32 a1_4, fp32 a1_5, fp32 a1_6, fp32 a1_7, fp32 a1_8,
  fp32 b0, fp32 b1, fp32 y0, fp32 y1){
  int solved = 1;
/*
  if (1){

    // (X) feasible

    //solved = solved && psd(X);

    //fp32 R[4] = {0,0,0,0};
    fp32 r00 = (fp32)0.0;
    fp32 r01 = (fp32)0.0;
    fp32 r10 = (fp32)0.0;
    fp32 r11 = (fp32)0.0;

    //i = 0
      //j=0
    fp32 s = (fp32)0.0;
    fp32 to_root = x0 - s;
    if (to_root < (fp32)(0.0)){
      solved = 0;
    } else {
      fp32 root = sqrt_val(to_root);
      r00 = root;
    }


      //j=0
    // = 1
    s = (fp32)0.0;
    r10 = (fp32)(1.0) / r00 * (x2 - s);


      //j=1
    s = r10 * r10;
    to_root = x3 - s;
    if (to_root < (fp32)(0.0)){
            solved = 0;
     } else {
            fp32 root = sqrt_val(to_root);
            r11 = root;
     }

      fp32 dot_s0f = (a0_0*x0) + (a0_1*x1) + (a0_2*x2) + (a0_3*x3);
      fp32 dot_s1f = (a1_0*x0) + (a1_1*x1) + (a1_2*x2) + (a1_3*x3);

      solved = solved && d_equal(dot_s0f,b0);
      solved = solved && d_equal(dot_s0f,b1);

      //}

      // (y,S) feasible
      //Matrix S = mat_sub(C, mat_comb(y,A)); // sum from 1 to m of yi*Ai
      //comb - Matrix sum = scal_mul(A.m0, y.v[0]);
      //sum = mat_add(sum,scal_mul(A.m1, y.v[1]));
    //    Matrix S = {N,N,{0,0,0,0}};
    //    for (int i = 0; i < S.rows*S.cols; i++){
      fp32 s0 = c0 - ((a0_0 * y0) + (a1_0 * y1));
      fp32 s1 = c1 - ((a0_1 * y0) + (a1_1 * y1));
      fp32 s2 = c2 - ((a0_2 * y0) + (a1_2 * y1));
      fp32 s3 = c3 - ((a0_3 * y0) + (a1_3 * y1));

    //    }

    //    solved = solved && psd(S);
      //fp32 R[4] = {0,0,0,0};
      fp32 r00s = (fp32)0.0;
      fp32 r01s = (fp32)0.0;
      fp32 r10s = (fp32)0.0;
      fp32 r11s = (fp32)0.0;

      //i = 0
        //j=0
      fp32 ss = (fp32)0.0;
      fp32 to_roots = x0 - ss;
      if (to_roots < (fp32)(0.0)){
        solved = 0;
      } else {
        fp32 roots = sqrt_val(to_roots);
        r00s = roots;
      }
        //j=0
      // = 1
      ss = (fp32)0.0;
      r10s = (fp32)(1.0) / r00s * (x2 - ss);


        //j=1
      ss = r10s * r10s;
      to_roots = x3 - ss;
      if (to_roots < (fp32)(0.0)){
              solved = 0;
       } else {
              fp32 roots = sqrt_val(to_roots);
              r11s = roots;
       }

      // C*X - sum(yi*bi) = 0 (duality gap = 0)
    fp32 gap = (s0*x0) + (s1*x1) + (s2*x2) + (s3*x3); // = dot(S,X);
    //dot(P.C,P.X) - vec_comb(y,b);

    solved = solved && (d_equal(gap,0.0));

  }

  return solved;
  */
  return 1;
}


int test(){
/*
  double C[9] = {-0.99154214,  0.65386878, -0.64033738, 0.65386878,  0.93792596, -0.14210919, -0.64033738, -0.14210919,  0.30795382};
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


  fp32 x0 = 0;// = __GADGET_exist();
  fp32 x1 = 0;// __GADGET_exist();
  fp32 x2 = 0;// __GADGET_exist();
  fp32 x3 = 0;// __GADGET_exist();
  /*
  fp32 x4 = __GADGET_exist();
  fp32 x5 = __GADGET_exist();
  fp32 x6 = __GADGET_exist();
  fp32 x7 = __GADGET_exist();
  fp32 x8 = __GADGET_exist();
  */

  fp32 y0 = 0;//= __GADGET_exist();
  fp32 y1 = 0;//= __GADGET_exist();

//n,m, C, X, big array of A's, b, sol_y, sol_x
 //__GADGET_sdp(2,2,-0.1983367,  0.54620727, 0.54620727,  0.29183634,1.3713053, 0.16070848,0.16070848, 1.43693619,-0.99890972, 0.14410886,0.14410886, -0.73737868,0.14047667, -0.17714865,-0.17714865,  0.49857682,-2.3830572764539832, 0.8521208961278653);


  int check = __GADGET_check(hw(2.0));//check_sdp(3,2,-0.1983367,  0.54620727, 0.54620727,  0.29183634, 0,0,0,0,0, x0,x1,x2,x3,0,0,0,0,0,-0.99890972, 0.14410886,0.14410886, -0.73737868,0,0,0,0,0, 0.14047667, -0.17714865,-0.17714865,  0.49857682,0,0,0,0,0, -2.3830572764539832, 0.8521208961278653, y0,y1));

  return check;

}
