int bar(int* xs) {
  return xs[1];
}

int main() {
  int v[3] = {4, 5, 6};
  return bar(v);
}

