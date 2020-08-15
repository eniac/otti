
int foo () {
  int a = 7;
  int * ptr = &a;
  *ptr = 100;
  return *ptr;
}
