void foo2(int * x) {
  *x += 1;
}

int foo() {
  int x = 0;
  foo2(&x);
  return x;
}
