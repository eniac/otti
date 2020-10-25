// #define BN_ULONG unsigned long
#define BN_ULONG uint32_t
#define BN_UMULT_LOHI(low,high,a,b) ({  \
        uint64_t ret=(uint64_t)(a)*(b); \
        (high)=ret>>32; (low)=ret;      })
//      __uint128_t ret=(__uint128_t)(a)*(b); \

//Broken
#define mul_add_c2(a,b,c0,c1,c2) {     \
       BN_ULONG ta=(a),tb=(b),t0,t1,t2;\
       BN_UMULT_LOHI(t0,t1,ta,tb);     \
       t2 = t1+t1; c2 += (t2<t1)?1:0;  \
       t1 = t0+t0; t2 += (t1<t0)?1:0;  \
       c0 += t1; t2 += (c0<t1)?1:0;    \
       c1 += t2; c2 += (c1<t2)?1:0;    \
       }


//Okay
//#define mul_add_c2(a,b,c0,c1,c2) {     \
//       BN_ULONG ta=(a),tb=(b),t0,t1,t2;\
//       BN_UMULT_LOHI(t0,t1,ta,tb);     \
//       c0 += t0; t2 = t1+((c0<t0)?1:0);\
//       c1 += t2; c2 += (c1<t2)?1:0;    \
//       c0 += t0; t1 += (c0<t0)?1:0;    \
//       c1 += t1; c2 += (c1<t1)?1:0;    \
//       }



#include "stdio.h"
#include "stdint.h"

int mult(uint32_t a, uint32_t b, uint32_t c0, uint32_t c1, uint32_t c2) {
    // Copy inputs so we can check correctness w.r.t. them
    uint32_t aa = a, bb = b, cc0 = c0, cc1 = c1, cc2 = c2;
    // Use macro
    mul_add_c2(a, b, c0, c1, c2);
    // Assert correctness
    // (c2 || c1 || c0) == (cc2 || cc1 || cc0) + 2 * aa * bb
    "__SMT_assert: Eq (DynBvNaryExpr BvAdd 96 [ DynBvConcat 96 (DynBvConcat 64 (Var \"cc2\" (SortBv 32)) (Var \"cc1\" (SortBv 32))) (Var \"cc0\" (SortBv 32)), DynBvNaryExpr BvMul 96 [ DynBvLit [96]2, DynBvUext 96 64 (Var \"aa\" (SortBv 32)), DynBvUext 96 64 (Var \"bb\" (SortBv 32))]]) (DynBvConcat 96 (DynBvConcat 64 (Var \"c2\" (SortBv 32)) (Var \"c1\" (SortBv 32))) (Var \"c0\" (SortBv 32)))";
    return 0;
}
