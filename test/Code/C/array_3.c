#define LEN 3
int main() {
  int v[LEN];
  for (int i = 0; i < LEN; ++i) v[i] = i + 1;
  int acc = 0;
  for (int j = 0; j < LEN; ++j) acc += v[j];
  return acc;
}
