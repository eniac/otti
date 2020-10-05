#include <stdint.h>

/*
  Matrix multiplication.
 */

//Constants
#define IDX(r,c) (SIZE*(r) + (c))

struct In {
  uint16_t A[9]; uint16_t B[9];
};

struct Out {
  uint64_t C[9];
};

void compute(struct In* input, struct Out* output) {
  int i,j,k;

  for(i = 0; i < 3; i++){
    for(j = 0; j < 3; j++){
      uint64_t C_ij = 0;
      for(k = 0; k < 3; k++){
        C_ij += input->A[3*i+k] * input->B[3*k+j];
      }
      output->C[3*i+j] = C_ij;
    }
  }
}
