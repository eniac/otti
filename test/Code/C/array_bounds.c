int trivial(int x) {
  int v[8] = {1, 2, 3, 4, 5, 6, 7, 8};
  return v[x];
}

int very_easy(int x) {
  int v[8] = {1, 2, 3, 4, 5, 6, 7, 8};
  // shifted bounds-check doesn't work
  if (x > 1) return v[(x + 1) % 8 - 1];
  else return v[x % 8];
}

int easy(int x) {
  int v[8] = {1, 2, 3, 4, 5, 6, 7, 8};
  int y = x >> (sizeof(int) * 8 - 3);
  return v[y];
}

int easy_okay(unsigned x) {
  int v[8] = {1, 2, 3, 4, 5, 6, 7, 8};
  int y = x >> (unsigned)(sizeof(int) * 8 - 3);
  return v[y];
}
