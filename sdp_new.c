typedef float fp64;

#define epsilon (fp64)1.0e-2


int d_equal(fp64 a, fp64 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b);
  }
}


//n,m, C, X, big array of A's, b, sol_y, sol_x
int check_sdp(int n,int m,fp64 c0,fp64 c1,fp64 c2,fp64 c3,fp64 c4,fp64 c5,fp64 c6,fp64 c7,fp64 c8,fp64 x0,fp64 x1,fp64 x2,fp64 x3,fp64 x4,fp64 x5,fp64 x6,fp64 x7,fp64 x8,fp64 a0_0,fp64 a0_1,fp64 a0_2,fp64 a0_3,fp64 a0_4,fp64 a0_5,fp64 a0_6,fp64 a0_7,fp64 a0_8,fp64 a1_0,fp64 a1_1,fp64 a1_2,fp64 a1_3,fp64 a1_4,fp64 a1_5,fp64 a1_6,fp64 a1_7,fp64 a1_8,fp64 b0,fp64 b1,fp64 y0,fp64 y1){

  int solved = 1;

  if (1){ // x feasible

solved = solved && (x0 > -0.01);
solved = solved && (x4 > -0.01);
solved = solved && (x8 > -0.01);
fp64 dx_r2_c2 = (x0*x4) - (x1*x3);
solved = solved && (dx_r2_c2 > -0.01);
fp64 dx_r1_c1 = (x0*x8) - (x2*x6);
solved = solved && (dx_r1_c1 > -0.01);
fp64 dx_r0_c0 = (x4*x8) - (x5*x7);
solved = solved && (dx_r0_c0 > -0.01);
fp64 dx_r0_c1 = (x3*x8) - (x5*x6);
fp64 dx_r0_c2 = (x3*x7) - (x4*x6);
fp64 dx = (x0 * dx_r0_c0) + (x1 * dx_r0_c1 * -1) + (x2 * dx_r0_c2);
solved = solved && (dx > -0.01);



fp64 dot_s0f = (a0_0*x0) + (a0_1*x1) + (a0_2*x2) + (a0_3*x3) + (a0_4*x4) + (a0_5*x5) + (a0_6*x6) + (a0_7*x7) + (a0_8*x8);
solved = solved && d_equal(dot_s0f,b0);
fp64 dot_s1f = (a1_0*x0) + (a1_1*x1) + (a1_2*x2) + (a1_3*x3) + (a1_4*x4) + (a1_5*x5) + (a1_6*x6) + (a1_7*x7) + (a1_8*x8);
solved = solved && d_equal(dot_s1f,b1);


fp64 s0 = c0 - ((a0_0 * y0) + (a1_0 * y1));
fp64 s1 = c1 - ((a0_1 * y0) + (a1_1 * y1));
fp64 s2 = c2 - ((a0_2 * y0) + (a1_2 * y1));
fp64 s3 = c3 - ((a0_3 * y0) + (a1_3 * y1));
fp64 s4 = c4 - ((a0_4 * y0) + (a1_4 * y1));
fp64 s5 = c5 - ((a0_5 * y0) + (a1_5 * y1));
fp64 s6 = c6 - ((a0_6 * y0) + (a1_6 * y1));
fp64 s7 = c7 - ((a0_7 * y0) + (a1_7 * y1));
fp64 s8 = c8 - ((a0_8 * y0) + (a1_8 * y1));


solved = solved && (s0 > -0.01);
solved = solved && (s4 > -0.01);
solved = solved && (s8 > -0.01);
fp64 ds_r2_c2 = (s0*s4) - (s1*s3);
solved = solved && (ds_r2_c2 > -0.01);
fp64 ds_r1_c1 = (s0*s8) - (s2*s6);
solved = solved && (ds_r1_c1 > -0.01);
fp64 ds_r0_c0 = (s4*s8) - (s5*s7);
solved = solved && (ds_r0_c0 > -0.01);
fp64 ds_r0_c1 = (s3*s8) - (s5*s6);
fp64 ds_r0_c2 = (s3*s7) - (s4*s6);
fp64 ds = (s0 * ds_r0_c0) + (s1 * ds_r0_c1 * -1) + (s2 * ds_r0_c2);
solved = solved && (ds > -0.01);



fp64 gap = (s0*x0) + (s1*x1) + (s2*x2) + (s3*x3) + (s4*x4) + (s5*x5) + (s6*x6) + (s7*x7) + (s8*x8);

solved = solved && (d_equal(gap,0.0));

}

return solved;

}


int sdp_instance(){

fp64 x0 = 0;
fp64 x1 = 0;
fp64 x2 = 0;
fp64 x3 = 0;
fp64 x4 = 0;
fp64 x5 = 0;
fp64 x6 = 0;
fp64 x7 = 0;
fp64 x8 = 0;


fp64 y0 = 0;
fp64 y1 = 0;


//n,m, C, X, big array of A's, b, sol_y, sol_x
__GADGET_sdp(3,2,-0.99154214,0.65386878,-0.64033738,0.65386878,0.93792596,-0.14210919,-0.64033738,-0.14210919,0.30795382,1.78032993,-0.00358676,0.72814533,-0.00358676,1.22372938,-0.05358303,0.72814533,-0.05358303,1.84385819,0.35184943,-0.38338125,0.31362847,-0.38338125,0.85702573,-0.58909452,0.31362847,-0.58909452,0.97137504,-0.4945009,-0.62078468,0.20384815,-0.62078468,0.21578012,-0.09717294,0.20384815,-0.09717294,-0.42178768,3.9888687923835366,-1.0823059466682765);

int check = __GADGET_check(check_sdp(3,2,-0.99154214,0.65386878,-0.64033738,0.65386878,0.93792596,-0.14210919,-0.64033738,-0.14210919,0.30795382,x0,x1,x2,x3,x4,x5,x6,x7,x8,0.35184943,-0.38338125,0.31362847,-0.38338125,0.85702573,-0.58909452,0.31362847,-0.58909452,0.97137504,-0.4945009,-0.62078468,0.20384815,-0.62078468,0.21578012,-0.09717294,0.20384815,-0.09717294,-0.42178768,3.9888687923835366,-1.0823059466682765,y0,y1));

return check;

}
