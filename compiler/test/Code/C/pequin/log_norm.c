#include <stdint.h>

struct In {
  uint32_t a;
};

struct Out {
  uint32_t loga;
};

//Returns floor(log2(a)), only works for nonzero a.
uint32_t uintlog2(uint32_t a){
  uint32_t result = 0;
  for (int i = 0; i < 32; ++i) {
    if ((a & (1 << i)) != 0) {
      result = i;
    }
  }
  return result;
}

void compute(struct In* input, struct Out* output){
  output->loga = uintlog2(input->a); 
}
