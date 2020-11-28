int round () {

  typedef signed int fixed_point_precision;

  // round down for now, could implement other modes later

  int a = 39;
  fixed_point_precision b = (fixed_point_precision)a;
  int c = (int)b;

  int d = 38;
  fixed_point_precision e = (fixed_point_precision)d;
  int f = (int)e;

  int g = -39;
  fixed_point_precision h = (fixed_point_precision)g;
  int i = (int)h;

  int j = -38;
  fixed_point_precision k = (fixed_point_precision)j;
  int l = (int)k;

  fixed_point_precision m = 39.5;
  int n = (int)m;

  // doubles rounding is weird and hardcoded into the TySmt layer

  double o = 38.5;
  fixed_point_precision p = (fixed_point_precision)o;
  int q = (int)p;

  float r = -39.5;
  fixed_point_precision s = (fixed_point_precision)r;
  int t = (int)s;

  int u = -38.5;
  fixed_point_precision v = (fixed_point_precision)u;
  int w = (int)v;


}
