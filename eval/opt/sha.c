
#include "stdint.h"

struct In {uint32_t msg[16];};



struct Out {uint32_t sha1[5];};

uint32_t LROT(uint32_t x, uint32_t m){ return (x << m) | (x >> (32 - m));
}

void compute(struct In *input, struct Out *output){
  int i;
  uint32_t w[80];
  uint32_t a,b,c,d,e,f,k,temp;
  uint32_t h[5];


  for(i = 0; i < 16; i++){
    w[i] = input->msg[i];
  }

  for(i = 16; i < 80; i++){
    w[i] = LROT(w[i-3] ^ w[i-8] ^ w[i-14] ^ w[i-16], 1);
  }

  h[0] = 0x67452301;
  h[1] = 0xEFCDAB89;
  h[2] = 0x98BADCFE;
  h[3] = 0x10325476;
  h[4] = 0xC3D2E1F0;


  a = h[0];
  b = h[1];
  c = h[2];
  d = h[3];
  e = h[4];


  for(i = 0; i < ROUNDS; i++){
    if (0 <= i && i <= 19){
      f = (b & c) | ((~b) & d);
      k = 0x5A827999;
    }
    if (20 <= i && i <= 39){
      f = b ^ c ^ d;
      k = 0x6ED9EBA1;
    }
    if (40 <= i && i <= 59){
      f = (b & c) | (b & d) | (c & d);
      k = 0x8F1BBCDC;
    }
    if (60 <= i && i <= 79){
      f = b ^ c ^ d;
      k = 0xCA62C1D6;
    }

    temp = (uint32_t)(LROT(a,5) + f + e + k + w[i]);
    e = d;
    d = c;
    c = LROT(b,30);
    b = a;
    a = temp;
  }


  h[0] = (uint32_t)(h[0] + a);
  h[1] = (uint32_t)(h[1] + b);
  h[2] = (uint32_t)(h[2] + c);
  h[3] = (uint32_t)(h[3] + d);
  h[4] = (uint32_t)(h[4] + e);
  __VERIFIER_assert((h[0] & 0x1) != 0);

  for(i = 0; i < 5; i++){
    output->sha1[i] = h[i];
  }
}

int main () {
  struct In in = { { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 } };
  struct Out out;
  compute(&in, &out);
  uint32_t zero = out.sha1[0];
  uint32_t one = out.sha1[1];
  uint32_t two = out.sha1[2];
  uint32_t three = out.sha1[3];
  uint32_t four = out.sha1[4];
  return 0;
}
