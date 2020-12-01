
int add () {

  typedef double fixed_point_precision_16_16;
  fixed_point_precision_16_16 x = 1.0;
  fixed_point_precision_16_16 y = 0.5;
  fixed_point_precision_16_16 z = x+y;

}

int sub() {

  typedef double fixed_point_precision_16_16;
  fixed_point_precision_16_16 x = 2.0;
  fixed_point_precision_16_16 y = 0.5;
  fixed_point_precision_16_16 z = x-y;

}

int mult() {

  typedef double fixed_point_precision_16_16;

  int w = 3;
  fixed_point_precision_16_16 x = 7.7;
  fixed_point_precision_16_16 y = 0.5;
  fixed_point_precision_16_16 z1 = x*y;
  fixed_point_precision_16_16 z2 = x*w;
  fixed_point_precision_16_16 z3 = w*y;

}

int div() {

  typedef double fixed_point_precision_16_16;

  int w = 9;
  fixed_point_precision_16_16 x = 14.5;
  fixed_point_precision_16_16 y = 3.5;
  fixed_point_precision_16_16 z1 = x/y;
  fixed_point_precision_16_16 z2 = x/w;
  fixed_point_precision_16_16 z3 = w/y;

}
