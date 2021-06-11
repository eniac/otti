int comp () {

  typedef float fp32;

  fp32 a = 978.382;
  fp32 b = -100.09;
  int c = -400;
  int d = 39;
  fp32 e = (fp32)d;
  int f = (int)e;

  int y = 0;
  if (a < (fp32)1000.0) {
      y += 1;
  }

  if (a < (fp32)1000) {
      y += 1;
  }

  if (b < e) {
      y += 1;
  }

  if ((fp32)c < b) {
      y += 1;
  }



}
