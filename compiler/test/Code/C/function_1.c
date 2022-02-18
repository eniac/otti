

int bar (int x, int y) {
  return x + y;
}

int foo (int x, int y) {
  return 2 * bar(x, y);
}

int main () {
  return foo(1,2);
}
