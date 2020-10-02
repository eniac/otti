int foo(int x) {
  int y;
  // return is undef b/c y is uninit.
  return x ? y : 0;
}

int bar(int x) {
  int y;
  // return is also undef b/c could divide by zero.
  return x / y;
}
