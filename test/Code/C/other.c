
int undef1 () {
  int x;
  return x; 
}

int undef2 () {
  int x;
  int y = 4;
  int z = y + x;
  return z;
}

int null (int * p) {
  p = ((void *)0);
  return *p;
}

int oob () {
  int x[3] = { 1, 2, 3 };
  return x[5];
}


int str () {
  char * x = "hello there";
  x[1] = 'o';
  return 0;
}
