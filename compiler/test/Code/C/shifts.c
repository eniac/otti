int shift_it (int x, int y) {
  int n = x << y;
  int m = n + x;
  return x >> y;
}
