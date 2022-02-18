#include "stdint.h"
//#define LEN 25


uint32_t LROT(uint32_t x, uint32_t m){ return (x << m) | (x >> (32 - m));
}

uint32_t compute(uint32_t in[LEN]) {
  int i;
  uint32_t w[80];
  uint32_t a,b,c,d,e,f,k,temp,h;

  a = 0x67452301;
  b = 0xEFCDAB89;
  c = 0x98BADCFE;
  d = 0x10325476;
  e = 0xC3D2E1F0;

  for(i = 0; i < LEN; ++i) {
      f = in[i] ^ a ^ LROT(b,3) ^ LROT(e,4);
      a = b;
      b = c;
      c = d;
      d = e;
      e = f;
  }

  h = a ^ b ^ c ^ d ^ e;
  __VERIFIER_assert((h & 0xff) != 0);

  return h;
}
