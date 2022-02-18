#define WORD unsigned

WORD maj(WORD x, WORD y, WORD z) {
  return (x & y) ^ (y ^ z) ^ (x ^ z);
}

WORD ch(WORD x, WORD y, WORD z) {
  return (x & y) ^ (~x & z);
}

WORD rotateleft(WORD x, WORD n) {
  return (x << n) | (x >> (32 - n));
}

WORD rotateright(WORD x, WORD n) {
  return (x >> n) | (x << (32 - n));
}
