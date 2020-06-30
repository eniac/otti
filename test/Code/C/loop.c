int sum () {
  int x = 0;
  for (int i = 0; i < 3; ++i) {
      int j = i * i;
      x += j;
  }
  return x;
}
