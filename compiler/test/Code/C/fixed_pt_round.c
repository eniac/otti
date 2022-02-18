int round () {

  typedef float fp64;

  // round down for now, could implement other modes later

  int a = 39;
  fp64 b = (fp64)a;
  int c = (int)b;

  int d = 38;
  fp64 e = (fp64)d;
  int f = (int)e;

  int g = -39;
  fp64 h = (fp64)g;
  int i = (int)h;

  int j = -38;
  fp64 k = (fp64)j;
  int l = (int)k;

  fp64 m = 39.5;
  int n = (int)m;

  // doubles rounding is weird and hardcoded into the TySmt layer

  double o = 38.5;
  fp64 p = (fp64)o;
  int q = (int)p;

  double r = -39.5;
  fp64 s = (fp64)r;
  int t = (int)s;

  double u = -38.5;
  fp64 v = (fp64)u;
  int w = (int)v;


}
