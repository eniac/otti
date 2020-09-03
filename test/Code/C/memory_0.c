
int foo () {
  int a = 7;
  int * ptr = &a;
  return *ptr;
}

int bar2 (int * b) {
  *b = 8;
}

// Guards in the caller affect operations in the callee
int bar () {
  int a = 7;
  int * ptr = &a;
  if (a < 7) {
    bar2(ptr);
  }
  // 7
  return *ptr;
}

int baz2 (_Bool c, int * b) {
  if (c) return 0;
  *b = 8;
  return 1;
}

// Early returns block memory ops
int baz () {
  int a = 7;
  int * ptr = &a;
  baz2(1, ptr);
  // 7
  return *ptr;
}
