typedef char bool;

// Square root from test/Code/C/sqrt.c
unsigned int uintsqrt(unsigned int x){
  unsigned int lo = 0;

  int i;
  unsigned int bit = 0x8000;
  for(i = 0; i < 16; i++){
    unsigned int mid = lo + bit;

    if (mid * mid > x){

    } else {
      lo = mid;
    }

    bit = bit / 2;
  }

  return lo;
}

// Verifier check code
bool max_check(int a, int b, int out) {
  int q = uintsqrt((a-b)*(a-b));
  return(((q >> 31) == 0) && (((a + b + q) / 2) == out));
}

// max gadget
int max(int a, int b) {
  // Prover code
  int out = __GADGET_compute((a > b) ? a : b);

  // Pre evaluating this
  __GADGET_rewrite(max_check(a, b, out));
  return out;
}

