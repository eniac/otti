template fork(n, m) {
    signal private input in;
    signal output out[n][m];

    for (var i = 0; i < n; ++i) {
        for (var j = 0; j < m; ++j) {
            out[i][j] <== in;
        }
    }
}

component main = fork(3,2);

