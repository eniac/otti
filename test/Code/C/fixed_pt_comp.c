int comp () {

  typedef float fixed_point_precision_16_16;

  fixed_point_precision_16_16 a = 978.382;
  fixed_point_precision_16_16 b = -100.09;
  int c = -400;
  int d = 39;
  fixed_point_precision_16_16 e = (fixed_point_precision_16_16)d;
  int f = (int)e;

  int y = 0;
  if (a < (fixed_point_precision_16_16)1000.0) {
      y += 1;
  }

  if (a < (fixed_point_precision_16_16)1000) {
      y += 1;
  }

  if (b < e) {
      y += 1;
  }

  if ((fixed_point_precision_16_16)c < b) {
      y += 1;
  }



}
