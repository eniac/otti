function double(n) {
    return 2 * n;
}

template fork(n) {
    signal input in[n];
    signal output out[double(n)];

    for (var i = 0; i < n; ++i) {
        out[2 * i + 0] <== in[i];
        out[2 * i + 1] <== in[i];
    }
}

component main = fork(3);

