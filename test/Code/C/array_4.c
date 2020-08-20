#define LEN 3
int main() {
  int v[LEN];
  for (int i = 0; i < LEN; ++i) v[i] = i + 1;
  int a = 5;
  if (a < 10) {
      v[1] = 4;
  }
  return v[1];
}
