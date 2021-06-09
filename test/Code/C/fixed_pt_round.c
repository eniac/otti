int round () {

  typedef float fp32;

  // round down for now, could implement other modes later

  int a = 39;
  fp32 b = (fp32)a;
  int c = (int)b;

  int d = 38;
  fp32 e = (fp32)d;
  int f = (int)e;

  int g = -39;
  fp32 h = (fp32)g;
  int i = (int)h;

  int j = -38;
  fp32 k = (fp32)j;
  int l = (int)k;

  fp32 m = 39.5;
  int n = (int)m;

  // doubles rounding is weird and hardcoded into the TySmt layer

  double o = 38.5;
  fp32 p = (fp32)o;
  int q = (int)p;

  float r = -39.5;
  fp32 s = (fp32)r;
  int t = (int)s;

  int u = -38.5;
  fp32 v = (fp32)u;
  int w = (int)v;


}
