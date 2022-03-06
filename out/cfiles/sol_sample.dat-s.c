typedef float fp64;
#define epsilon (fp64)1.0e-2

int d_equal(fp64 a, fp64 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b) < epsilon;
  }
}

int check_sdp(int n,int m,fp64 c0,fp64 c1,fp64 c2,fp64 c3,fp64 c4,fp64 c5,fp64 c6,fp64 c7,fp64 c8,fp64 c9,fp64 c10,fp64 c11,fp64 c12,fp64 c13,fp64 c14,fp64 c15,fp64 x0,fp64 x1,fp64 x2,fp64 x3,fp64 x4,fp64 x5,fp64 x6,fp64 x7,fp64 x8,fp64 x9,fp64 x10,fp64 x11,fp64 x12,fp64 x13,fp64 x14,fp64 x15,fp64 a0_0,fp64 a0_1,fp64 a0_2,fp64 a0_3,fp64 a0_4,fp64 a0_5,fp64 a0_6,fp64 a0_7,fp64 a0_8,fp64 a0_9,fp64 a0_10,fp64 a0_11,fp64 a0_12,fp64 a0_13,fp64 a0_14,fp64 a0_15,fp64 a1_0,fp64 a1_1,fp64 a1_2,fp64 a1_3,fp64 a1_4,fp64 a1_5,fp64 a1_6,fp64 a1_7,fp64 a1_8,fp64 a1_9,fp64 a1_10,fp64 a1_11,fp64 a1_12,fp64 a1_13,fp64 a1_14,fp64 a1_15,fp64 b0,fp64 b1,fp64 y0,fp64 y1,fp64 xq0,fp64 xq4,fp64 xq5,fp64 xq8,fp64 xq9,fp64 xq10,fp64 xq12,fp64 xq13,fp64 xq14,fp64 xq15,fp64 sq0,fp64 sq4,fp64 sq5,fp64 sq8,fp64 sq9,fp64 sq10,fp64 sq12,fp64 sq13,fp64 sq14,fp64 sq15){
int solved = 1;

fp64 dot_b0 = (a0_0*x0) + (a0_1*x1) + (a0_2*x2) + (a0_3*x3) + (a0_4*x4) + (a0_5*x5) + (a0_6*x6) + (a0_7*x7) + (a0_8*x8) + (a0_9*x9) + (a0_10*x10) + (a0_11*x11) + (a0_12*x12) + (a0_13*x13) + (a0_14*x14) + (a0_15*x15);
fp64 dot_b1 = (a1_0*x0) + (a1_1*x1) + (a1_2*x2) + (a1_3*x3) + (a1_4*x4) + (a1_5*x5) + (a1_6*x6) + (a1_7*x7) + (a1_8*x8) + (a1_9*x9) + (a1_10*x10) + (a1_11*x11) + (a1_12*x12) + (a1_13*x13) + (a1_14*x14) + (a1_15*x15);


fp64 xq1 = 0.0;
fp64 xq2 = 0.0;
fp64 xq3 = 0.0;
fp64 xq6 = 0.0;
fp64 xq7 = 0.0;
fp64 xq11 = 0.0;
fp64 xr0 = xq0;
fp64 xr1 = xq4;
fp64 xr2 = xq8;
fp64 xr3 = xq12;
fp64 xr4 = xq1;
fp64 xr5 = xq5;
fp64 xr6 = xq9;
fp64 xr7 = xq13;
fp64 xr8 = xq2;
fp64 xr9 = xq6;
fp64 xr10 = xq10;
fp64 xr11 = xq14;
fp64 xr12 = xq3;
fp64 xr13 = xq7;
fp64 xr14 = xq11;
fp64 xr15 = xq15;
fp64 xm0 = (xq0 * xr0) +(xq1 * xr4) +(xq2 * xr8) +(xq3 * xr12);
fp64 xm1 = (xq0 * xr1) +(xq1 * xr5) +(xq2 * xr9) +(xq3 * xr13);
fp64 xm2 = (xq0 * xr2) +(xq1 * xr6) +(xq2 * xr10) +(xq3 * xr14);
fp64 xm3 = (xq0 * xr3) +(xq1 * xr7) +(xq2 * xr11) +(xq3 * xr15);
fp64 xm4 = (xq4 * xr0) +(xq5 * xr4) +(xq6 * xr8) +(xq7 * xr12);
fp64 xm5 = (xq4 * xr1) +(xq5 * xr5) +(xq6 * xr9) +(xq7 * xr13);
fp64 xm6 = (xq4 * xr2) +(xq5 * xr6) +(xq6 * xr10) +(xq7 * xr14);
fp64 xm7 = (xq4 * xr3) +(xq5 * xr7) +(xq6 * xr11) +(xq7 * xr15);
fp64 xm8 = (xq8 * xr0) +(xq9 * xr4) +(xq10 * xr8) +(xq11 * xr12);
fp64 xm9 = (xq8 * xr1) +(xq9 * xr5) +(xq10 * xr9) +(xq11 * xr13);
fp64 xm10 = (xq8 * xr2) +(xq9 * xr6) +(xq10 * xr10) +(xq11 * xr14);
fp64 xm11 = (xq8 * xr3) +(xq9 * xr7) +(xq10 * xr11) +(xq11 * xr15);
fp64 xm12 = (xq12 * xr0) +(xq13 * xr4) +(xq14 * xr8) +(xq15 * xr12);
fp64 xm13 = (xq12 * xr1) +(xq13 * xr5) +(xq14 * xr9) +(xq15 * xr13);
fp64 xm14 = (xq12 * xr2) +(xq13 * xr6) +(xq14 * xr10) +(xq15 * xr14);
fp64 xm15 = (xq12 * xr3) +(xq13 * xr7) +(xq14 * xr11) +(xq15 * xr15);
solved = solved && (d_equal(x0,xm0));
solved = solved && (d_equal(x1,xm1));
solved = solved && (d_equal(x2,xm2));
solved = solved && (d_equal(x3,xm3));
solved = solved && (d_equal(x4,xm4));
solved = solved && (d_equal(x5,xm5));
solved = solved && (d_equal(x6,xm6));
solved = solved && (d_equal(x7,xm7));
solved = solved && (d_equal(x8,xm8));
solved = solved && (d_equal(x9,xm9));
solved = solved && (d_equal(x10,xm10));
solved = solved && (d_equal(x11,xm11));
solved = solved && (d_equal(x12,xm12));
solved = solved && (d_equal(x13,xm13));
solved = solved && (d_equal(x14,xm14));
solved = solved && (d_equal(x15,xm15));


solved = solved && d_equal(dot_b0,b0);
solved = solved && d_equal(dot_b1,b1);


fp64 s0 = c0 - ((a0_0 * y0) + (a1_0 * y1));
fp64 s1 = c1 - ((a0_1 * y0) + (a1_1 * y1));
fp64 s2 = c2 - ((a0_2 * y0) + (a1_2 * y1));
fp64 s3 = c3 - ((a0_3 * y0) + (a1_3 * y1));
fp64 s4 = c4 - ((a0_4 * y0) + (a1_4 * y1));
fp64 s5 = c5 - ((a0_5 * y0) + (a1_5 * y1));
fp64 s6 = c6 - ((a0_6 * y0) + (a1_6 * y1));
fp64 s7 = c7 - ((a0_7 * y0) + (a1_7 * y1));
fp64 s8 = c8 - ((a0_8 * y0) + (a1_8 * y1));
fp64 s9 = c9 - ((a0_9 * y0) + (a1_9 * y1));
fp64 s10 = c10 - ((a0_10 * y0) + (a1_10 * y1));
fp64 s11 = c11 - ((a0_11 * y0) + (a1_11 * y1));
fp64 s12 = c12 - ((a0_12 * y0) + (a1_12 * y1));
fp64 s13 = c13 - ((a0_13 * y0) + (a1_13 * y1));
fp64 s14 = c14 - ((a0_14 * y0) + (a1_14 * y1));
fp64 s15 = c15 - ((a0_15 * y0) + (a1_15 * y1));


fp64 sq1 = 0.0;
fp64 sq2 = 0.0;
fp64 sq3 = 0.0;
fp64 sq6 = 0.0;
fp64 sq7 = 0.0;
fp64 sq11 = 0.0;
fp64 sr0 = sq0;
fp64 sr1 = sq4;
fp64 sr2 = sq8;
fp64 sr3 = sq12;
fp64 sr4 = sq1;
fp64 sr5 = sq5;
fp64 sr6 = sq9;
fp64 sr7 = sq13;
fp64 sr8 = sq2;
fp64 sr9 = sq6;
fp64 sr10 = sq10;
fp64 sr11 = sq14;
fp64 sr12 = sq3;
fp64 sr13 = sq7;
fp64 sr14 = sq11;
fp64 sr15 = sq15;
fp64 sm0 = (sq0 * sr0) +(sq1 * sr4) +(sq2 * sr8) +(sq3 * sr12);
fp64 sm1 = (sq0 * sr1) +(sq1 * sr5) +(sq2 * sr9) +(sq3 * sr13);
fp64 sm2 = (sq0 * sr2) +(sq1 * sr6) +(sq2 * sr10) +(sq3 * sr14);
fp64 sm3 = (sq0 * sr3) +(sq1 * sr7) +(sq2 * sr11) +(sq3 * sr15);
fp64 sm4 = (sq4 * sr0) +(sq5 * sr4) +(sq6 * sr8) +(sq7 * sr12);
fp64 sm5 = (sq4 * sr1) +(sq5 * sr5) +(sq6 * sr9) +(sq7 * sr13);
fp64 sm6 = (sq4 * sr2) +(sq5 * sr6) +(sq6 * sr10) +(sq7 * sr14);
fp64 sm7 = (sq4 * sr3) +(sq5 * sr7) +(sq6 * sr11) +(sq7 * sr15);
fp64 sm8 = (sq8 * sr0) +(sq9 * sr4) +(sq10 * sr8) +(sq11 * sr12);
fp64 sm9 = (sq8 * sr1) +(sq9 * sr5) +(sq10 * sr9) +(sq11 * sr13);
fp64 sm10 = (sq8 * sr2) +(sq9 * sr6) +(sq10 * sr10) +(sq11 * sr14);
fp64 sm11 = (sq8 * sr3) +(sq9 * sr7) +(sq10 * sr11) +(sq11 * sr15);
fp64 sm12 = (sq12 * sr0) +(sq13 * sr4) +(sq14 * sr8) +(sq15 * sr12);
fp64 sm13 = (sq12 * sr1) +(sq13 * sr5) +(sq14 * sr9) +(sq15 * sr13);
fp64 sm14 = (sq12 * sr2) +(sq13 * sr6) +(sq14 * sr10) +(sq15 * sr14);
fp64 sm15 = (sq12 * sr3) +(sq13 * sr7) +(sq14 * sr11) +(sq15 * sr15);
solved = solved && (d_equal(s0,sm0));
solved = solved && (d_equal(s1,sm1));
solved = solved && (d_equal(s2,sm2));
solved = solved && (d_equal(s3,sm3));
solved = solved && (d_equal(s4,sm4));
solved = solved && (d_equal(s5,sm5));
solved = solved && (d_equal(s6,sm6));
solved = solved && (d_equal(s7,sm7));
solved = solved && (d_equal(s8,sm8));
solved = solved && (d_equal(s9,sm9));
solved = solved && (d_equal(s10,sm10));
solved = solved && (d_equal(s11,sm11));
solved = solved && (d_equal(s12,sm12));
solved = solved && (d_equal(s13,sm13));
solved = solved && (d_equal(s14,sm14));
solved = solved && (d_equal(s15,sm15));


fp64 gap = (s0*x0) + (s1*x1) + (s2*x2) + (s3*x3) + (s4*x4) + (s5*x5) + (s6*x6) + (s7*x7) + (s8*x8) + (s9*x9) + (s10*x10) + (s11*x11) + (s12*x12) + (s13*x13) + (s14*x14) + (s15*x15);

solved = solved && (d_equal(gap,0.0));
return solved;
}

int main(){

fp64 x0 = __GADGET_exist();
fp64 x1 = __GADGET_exist();
fp64 x2 = __GADGET_exist();
fp64 x3 = __GADGET_exist();
fp64 x4 = __GADGET_exist();
fp64 x5 = __GADGET_exist();
fp64 x6 = __GADGET_exist();
fp64 x7 = __GADGET_exist();
fp64 x8 = __GADGET_exist();
fp64 x9 = __GADGET_exist();
fp64 x10 = __GADGET_exist();
fp64 x11 = __GADGET_exist();
fp64 x12 = __GADGET_exist();
fp64 x13 = __GADGET_exist();
fp64 x14 = __GADGET_exist();
fp64 x15 = __GADGET_exist();


fp64 y0 = __GADGET_exist();
fp64 y1 = __GADGET_exist();


fp64 xq0 = __GADGET_exist();
fp64 xq4 = __GADGET_exist();
fp64 xq5 = __GADGET_exist();
fp64 xq8 = __GADGET_exist();
fp64 xq9 = __GADGET_exist();
fp64 xq10 = __GADGET_exist();
fp64 xq12 = __GADGET_exist();
fp64 xq13 = __GADGET_exist();
fp64 xq14 = __GADGET_exist();
fp64 xq15 = __GADGET_exist();
fp64 sq0 = __GADGET_exist();
fp64 sq4 = __GADGET_exist();
fp64 sq5 = __GADGET_exist();
fp64 sq8 = __GADGET_exist();
fp64 sq9 = __GADGET_exist();
fp64 sq10 = __GADGET_exist();
fp64 sq12 = __GADGET_exist();
fp64 sq13 = __GADGET_exist();
fp64 sq14 = __GADGET_exist();
fp64 sq15 = __GADGET_exist();


__GADGET_sdp(4,2,"/Users/jesskwoods/docker_test/otti/datasets/SDP/small/sol_sample.dat-s",0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,-1.2809495358410854e-08,0.0,0.0,0.0,0.0,-1.5903809338044338e-08,0.0,0.0,0.0,0.0,-2.000000020593151,-2.0000000061886283,0.0,0.0,-2.0000000061886283,-2.0000000236874644,-5.6572347076032194,0.0,0.0,0.0,0.0,-4.34276529239678,0.0,0.0,0.0,0.0,-2.236748197130824,2.2367478365463405,0.0,0.0,2.2367478365463405,-2.2367475113557433,-1.0000000076879147,-1.0000000030943141);

int check = __GADGET_check(check_sdp(4,2,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,-1.2809495358410854e-08,0.0,0.0,0.0,0.0,-1.5903809338044338e-08,0.0,0.0,0.0,0.0,-2.000000020593151,-2.0000000061886283,0.0,0.0,-2.0000000061886283,-2.0000000236874644,-5.6572347076032194,0.0,0.0,0.0,0.0,-4.34276529239678,0.0,0.0,0.0,0.0,-2.236748197130824,2.2367478365463405,0.0,0.0,2.2367478365463405,-2.2367475113557433,-1.0000000076879147,-1.0000000030943141,y0,y1,xq0,xq4,xq5,xq8,xq9,xq10,xq12,xq13,xq14,xq15,sq0,sq4,sq5,sq8,sq9,sq10,sq12,sq13,sq14,sq15));

return check;
}