int main() {
  int v[3];
  for (int i = 0; i < 3; ++i) v[i] = i + 1;
  int acc = 0;
  for (int j = 0; j < 3; ++j) acc += v[j];
  return acc;
}
