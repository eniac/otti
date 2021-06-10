
int foo () {
  typedef float fp32;

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

  int five = 0 ? (_Bool)0 : (int)5;
  int six = (_Bool)0 + (int)6;

  float l = 15.4;
  fp32 m = (fp32)l;

  double n = -30.0;
  fp32 o = (fp32)n;

  short p = 104;
  fp32 q = (fp32)p;

  fp32 r = 10.9;
  int s = (int)r;

  fp32 t = -10.9;
  int u = (int)t;


}
