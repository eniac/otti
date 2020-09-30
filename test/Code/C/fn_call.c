int inner (int x, int y) {
    return x + y;
}

int outer (int x, int y) {
    return inner(x, y);
}
