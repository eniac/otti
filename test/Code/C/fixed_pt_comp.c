int comp () {

  typedef signed int fixed_point_precision;

  fixed_point_precision a = 978.382;
  fixed_point_precision b = -100.09;
  int c = -400;
  int d = 39;
  fixed_point_precision e = (fixed_point_precision)d;
  int f = (int)e;

  int y = 0;
  if (a < (fixed_point_precision)1000.0) {
      y += 1;
  }

  if (a < (fixed_point_precision)1000) {
      y += 1;
  }

  if (b < e) {
      y += 1;
  }

  if ((fixed_point_precision)c < b) {
      y += 1;
  }



}
