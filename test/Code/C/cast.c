
int foo () {
  typedef float fixed_point_precision_16_16;

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
  fixed_point_precision_16_16 m = (fixed_point_precision_16_16)l;
  double n = -30.0;
  fixed_point_precision_16_16 o = (fixed_point_precision_16_16)n;

  short p = 104;
  fixed_point_precision_16_16 q = (fixed_point_precision_16_16)p;

  fixed_point_precision_16_16 r = 10.9;
  int s = (int)r;

  fixed_point_precision_16_16 t = -10.9;
  int u = (int)t;

}
