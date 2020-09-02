// Tests for c pointers as references.

int apple() {
  int x = 5;
  int *p = &x;
  return *p;
  // 5
}

int banana() {
  int x = 5;
  int *p = &x;
  *p = 4;
  return *p;
  // 4
}

int cherry() {
  int x = 5;
  int *p = &x;
  for (int i = 0; i < 2; ++i) {
    *p = i;
  }
  return *p;
  // 1
}

int date2(int * x) {
  *x = 4;
  return 0;
}

int date() {
  int x = 5;
  date2(&x);
  return x;
  // 4
}

int global = 4;

int elderberry() {
  int * p = &global;
  *p = 5;
  return *p;
  // 5
}

int ite(_Bool c, int * x, int * y) {
  return c ? *x : *y;
}
int relu(int * x) {
  int z = 0;
  return ite(*x < 0, &z, x);
}
int fig() {
  int b = -1;
  return relu(&b);
  // 0
}

int grapefruit() {
  int x = 5;
  int *p = &x;
  for (int i = 0; i < 2; ++i) {
    int x = 0;
    *p = i;
  }
  return *p;
  // 1
}
