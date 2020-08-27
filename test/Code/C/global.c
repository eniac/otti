

int x = 4;
char xs[] = {4,5,6};

int foo () {
  return x;
}

char bar () {
  xs[0] = 10;
  return xs[0] + xs[1];
}
