int foo(int x, int y) {
    int xs[4] = {0,1,2,3};
    xs[x] = 2;
    return xs[y];
}

int flex(int x, int y) {
    int xs[4] = {0,1,2,3};
    if (x) {
      xs[x] = 2;
    }
    return xs[y];
}
