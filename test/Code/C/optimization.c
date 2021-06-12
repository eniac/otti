int maximize() {
  typedef double fp64;
  fp64 x,y;

  __GADGET_maximize(
      3 * x + 4 * y,   // objective
      x + 2 * y <= 14, // constraint 1
      3 * x - y >= 0,  // constraint 2
      x - y <= 2);     // constraint 3
}

int minimize() {
  typedef double fp64;
  fp64 x1,x2;

  __GADGET_minimize(
      x1 + 2*x2,   // objective
      50*x1 + 250*x2 >= 500,
      240*x1 + 80*x2 >= 960,
      x1 + x2 >= 6.0,
      x1 >= 0,
      x2 >= 0);
}
