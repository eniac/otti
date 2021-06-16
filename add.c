

int add(int a, int b) {

  double ex = __GADGET_exist();

  int c = __GADGET_compute(a + b);

//  __GADGET_check(a + b == c);

  return c;

}


