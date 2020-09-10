#include "stdint.h"

uint32_t uintsqrt(uint32_t x){
  uint32_t lo = 0;

  int i;
  uint32_t bit = 0x8000;
  for(i = 0; i < 16; i++){
    uint32_t mid = lo + bit;

    if (mid * mid > x){

    } else {
      lo = mid;
    }

    bit = bit / 2;
  }

  return lo;
}

int main () {
  uint32_t out = uintsqrt(9);
  return out;
}
