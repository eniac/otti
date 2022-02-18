int comp () {

  typedef float fp64;

  fp64 a = 978.382;
  fp64 b = -100.09;
  int c = -400;
  int d = 39;
  fp64 e = (fp64)d;
  int f = (int)e;

  int y = 0;
  if (a < (fp64)1000.0) {
      y += 1;
  }

  if (a < (fp64)1000) {
      y += 1;
  }

  if (b < e) {
      y += 1;
  }

  if ((fp64)c < b) {
      y += 1;
  }



}
