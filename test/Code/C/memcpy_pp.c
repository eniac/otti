typedef long unsigned int size_t;

int main() {
  return 0;
}

void *memcpy(void *dst, const void *src, size_t nbytes) {




    if(((unsigned)dst % 4 == 0) && ((unsigned)src % 4 == 0) && ((unsigned)nbytes % 4 == 0)) {
        unsigned n = nbytes / 4;
        unsigned *d = dst;
        const unsigned *s = src;

        for(unsigned i = 0; i < n; i++)
            d[i] = s[i];
    } else {
        unsigned char *d = dst;
        const unsigned char *s = src;
        for(unsigned i = 0; i < nbytes; i++)
            d[i] = s[i];
    }
    return dst;
}
