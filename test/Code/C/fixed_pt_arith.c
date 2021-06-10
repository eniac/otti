
int add () {

  typedef double fp32;
  fp32 x = 1.0;
  fp32 y = 0.5;
  fp32 z = x+y;

}

int sub() {

  typedef double fp32;
  fp32 x = 2.0;
  fp32 y = 0.5;
  fp32 z = x-y;

}

int mult() {

  typedef double fp32;

  int w = 3;
  fp32 x = 7.7;
  fp32 y = 0.5;
  fp32 z1 = x*y;
  fp32 z2 = x*w;
  fp32 z3 = w*y;

}

int div() {

  typedef double fp32;

  int w = 9;
  fp32 x = 14.5;
  fp32 y = 3.5;
  fp32 z1 = x/y;
  fp32 z2 = x/w;
  fp32 z3 = w/y;

}
