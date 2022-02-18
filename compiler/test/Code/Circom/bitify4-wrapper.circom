include "bitify.circom";

template bitifyWrapper(n) {
    signal input in;
    signal output out[n];
    component inner = Num2Bits(n);
    inner.in <== in;
    for (var i = 0; i < n; ++i) {
        out[i] <== inner.out[i];
    }
}

component main = bitifyWrapper(4);
