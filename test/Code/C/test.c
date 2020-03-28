
typedef long unsigned int size_t;
typedef int * intptr;

int bob (int*** bob, void *dst, const unsigned long int bytes, const void *src, unsigned nbytes) {
  unsigned *d = dst;
  *d = 4;
  return 5;    
}
