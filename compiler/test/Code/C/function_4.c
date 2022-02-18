
int foo (int x, int y) {
  for (int i = 0; i < x; i++) {
    y++;
  }
  return y;
}

int main () {
  return foo(5, 0);
}
