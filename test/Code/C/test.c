
typedef long unsigned int size_t;
typedef int * intptr;

int bob (int*** bob, void *dst, const unsigned long int bytes, const void *src, size_t nbytes) {
  int * baz, bin;
  int bax, * box;
  if(((unsigned)dst % 4 == 0) && ((unsigned)src % 4 == 0) && ((unsigned)nbytes % 4 == 0)) {
    unsigned n = nbytes / 4;
    unsigned *d = dst;
    const unsigned *s = src;
  }	
    
}
