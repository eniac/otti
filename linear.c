int main() {
  typedef double fp32;
  fp32 x,y;

  __GADGET_maximize(
      3 * x + 4 * y,   // objective
      x + 2 * y <= 14, // constraint 1
      3 * x - y >= 0,  // constraint 2
      x - y <= 2);     // constraint 3

  return x;
}

