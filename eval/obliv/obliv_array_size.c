#include "stdint.h"
int main() {
    uint32_t xs[LEN];
    int i;
    for (i = 0; i < LEN; ++i) {
        xs[i] = __VERIFIER_nondet_uint();
    }
    uint32_t acc = 0;
    for (i = 0; i < LEN; ++i) {
        acc += xs[i];
    }
    __VERIFIER_assert(acc != LEN);
}
