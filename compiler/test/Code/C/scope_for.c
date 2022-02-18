

int foo () {
  int x = 4;
  for (int i = 0; i < x; i++) {
    int x = 50;
    x++;
  }
  return x;
}
