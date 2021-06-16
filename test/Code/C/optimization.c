typedef double fp32;

#define epsilon (fp32)1.0e-2

int d_equal(fp32 a, fp32 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b) < epsilon;
  }
}

int max_check(fp32 x1, fp32 x2, fp32 y1, fp32 y2, fp32 y3){

  // duality gap
  fp32 xc = 3 * x1 + 4 * x2;
  fp32 yb = 14 * y1 + 2 * y3;
  int dual = d_equal(xc,yb);

  // primal sat
  int sat = (x1 + 2 * x2 <= 14);
  sat = sat && (-3 * x1 + x2 <= 0);
  sat = sat && (x1 - x2 <= 2);

  // dual sat
  sat = sat && (1 * y1 - 3 * y2 + 1 * y3 >= 2.99);
  sat = sat && (2 * y1 + 1 * y2 - 1 * y3 >= 3.99);

  return sat && dual;

}


int maximize() {
  fp32 x1,x2,y1,y2,y3,z;

  __GADGET_maximize(
      z,
      3 * x1 + 4 * x2 == z,   // objective
      x1 + 2 * x2 <= 14, // constraint 1
      -3 * x1 + x2 <= 0,  // constraint 2
      x1 - x2 <= 2,     // constraint 3

      // dual
      14 * y1 + 2 * y3 == z,
      1 * y1 - 3 * y2 + 1 * y3 >= 3,
      2 * y1 + 1 * y2 - 1 * y3 >= 4);


  __GADGET_check(max_check(x1,x2,y1,y2,y3));


}

int minimize() {
  fp32 x1,x2;

  __GADGET_minimize(
      x1 + 2*x2,   // objective
      50*x1 + 250*x2 >= 500,
      240*x1 + 80*x2 >= 960,
      x1 + x2 >= 6.0,
      x1 >= 0,
      x2 >= 0);
}
