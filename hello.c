#define epsilon (fp32)1.0e-2

typedef double fp32;




int d_equal(fp32 a, fp32 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b) < epsilon;
  }
}
