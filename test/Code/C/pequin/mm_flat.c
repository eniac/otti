#include <stdint.h>

/*
  Matrix multiplication.
 */

//Constants
#define IDX(r,c) (SIZE*(r) + (c))

struct In {
  uint16_t A[25]; uint16_t B[25];
};

struct Out {
  uint64_t C[25];
};

void compute(struct In* input, struct Out* output) {
  int i,j,k;

  for(i = 0; i < 5; i++){
    for(j = 0; j < 5; j++){
      uint64_t C_ij = 0;
      for(k = 0; k < 5; k++){
        C_ij += input->A[5*i+k] * input->B[5*k+j];
      }
      output->C[5*i+j] = C_ij;
    }
  }
}
