int max_naive(int a, int b) {
  return (a > b) ? a : b;
}

int max(int a, int b) {

  int result = max_naive(a,b);
  int q;

  __GADGET_rewrite((a-b) * (a-b) == q * q, // q = sqrt((a-b)^2)
		   (q >> 31) == 0,     // overflow check?
		   (a + b + q) / 2 == result);

  return result;
}

