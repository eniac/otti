

int foo (int x, int y) {
  if (x <= 0 && y <= 0) return 0;
  return foo (x-1, y-1);
}

int main() {
  return foo(2, 2);
}
