#define LEN 3
int bar(int xs[LEN]) {
  return xs[1];
}

int main() {
  int v[LEN];
  for (int i = 0; i < LEN; ++i) v[i] = i + 1;
  return bar(v);
}
