
int foo () {
  typedef double fixed_point_precision;

  int a = (_Bool)1;
  int b = (_Bool)2;
  int c = (_Bool)0;

  long d = -1;
  int e = (int)d;
  long f = 9223372036854775807;
  int g = (int)f;

  short h = 255;
  int i = (int)h;
  short j = 127;
  int k = (int)j;

  float l = 15.4;
  fixed_point_precision m = (fixed_point_precision)l;
  double n = -30.0;
  fixed_point_precision o = (fixed_point_precision)n;

  int p = 104;
  fixed_point_precision q = (fixed_point_precision)p;

}
