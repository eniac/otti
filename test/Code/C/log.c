#include <stdint.h>

struct In {
  uint32_t a;
};

struct Out {
  uint32_t loga;
};

//Returns floor(log2(a)), only works for nonzero a.
uint32_t uintlog2(uint32_t a){
  uint32_t one = 1u;
  int i;
  int ans = 0;
  _Bool done = 0;
  for(i = 0; i < 32; i++) {
    if (!done && (a & (one << (31 - i)))) {
      done = 1;
      ans = 31 - i;
    }
  }
  return ans;
}

void compute(struct In* input, struct Out* output){
  output->loga = uintlog2(input->a); 
}
