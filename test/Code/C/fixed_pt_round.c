int round () {

  typedef float fixed_point_precision_16_16;

  // round down for now, could implement other modes later

  int a = 39;
  fixed_point_precision_16_16 b = (fixed_point_precision_16_16)a;
  int c = (int)b;

  int d = 38;
  fixed_point_precision_16_16 e = (fixed_point_precision_16_16)d;
  int f = (int)e;

  int g = -39;
  fixed_point_precision_16_16 h = (fixed_point_precision_16_16)g;
  int i = (int)h;

  int j = -38;
  fixed_point_precision_16_16 k = (fixed_point_precision_16_16)j;
  int l = (int)k;

  fixed_point_precision_16_16 m = 39.5;
  int n = (int)m;

  // doubles rounding is weird and hardcoded into the TySmt layer

  double o = 38.5;
  fixed_point_precision_16_16 p = (fixed_point_precision_16_16)o;
  int q = (int)p;

  float r = -39.5;
  fixed_point_precision_16_16 s = (fixed_point_precision_16_16)r;
  int t = (int)s;

  int u = -38.5;
  fixed_point_precision_16_16 v = (fixed_point_precision_16_16)u;
  int w = (int)v;


}
