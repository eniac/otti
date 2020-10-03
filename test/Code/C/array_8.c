int sum(int xs[5]) {
    int acc = 0;
    for (int i = 0; i < 5; ++i) acc += xs[i];
    return acc;
}

typedef struct AS {
  int xs[5];
} A;

int struct_sum(A a) {
    int acc = 0;
    for (int i = 0; i < 5; ++i) acc += a.xs[i];
    return acc;
}
