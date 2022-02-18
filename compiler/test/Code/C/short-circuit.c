int short_or() {
  int x = 0;
  1 || (x = 1);
  // Should return 0
  return x;
}

int short_and() {
  int x = 0;
  0 && (x = 1);
  // Should return 0
  return x;
}

int main() {
  return short_or();
}
