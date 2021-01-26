// Credits to Jess Woods
int main() {
  int a = 0;
  if (a > 0){
    int p = 10 / a;
  }
  return a;
}

unsigned div(unsigned a, unsigned b) {
  return a / b;
}

unsigned maybe_div(unsigned a, unsigned b) {
  if (b == 0)
  {
      return a;
  }
  else
  {
      return a / b;
  }
}
