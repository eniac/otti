
int add () {

  typedef signed int fixed_point_precision;

  fixed_point_precision x = 1.0;

  fixed_point_precision y = 0.5;

  fixed_point_precision z = x+y;

}

int sub() {

  typedef signed int fixed_point_precision;

  fixed_point_precision x = 2.0;

  fixed_point_precision y = 0.5;

  fixed_point_precision z = x-y;

}

int mult() {

  typedef signed int fixed_point_precision;

  //int w = 3;

  fixed_point_precision x = 7.7;

  fixed_point_precision y = 0.5;

  fixed_point_precision z1 = x*y;

  //fixed_point_precision z2 = x*w;

  //fixed_point_precision z3 = w*y;

}

int div() {

  typedef signed int fixed_point_precision;

  //int w = 9;

  fixed_point_precision x = 14.5;

  fixed_point_precision y = 3.5;

  fixed_point_precision z1 = x/y;

  //fixed_point_precision z2 = x/w;

  //fixed_point_precision z3 = w/y;

}
