

int foo (int x, int y, int z, int w) {
  if (x == y == z == w) return 15;
  if (x == y) return 0;
  return 200;
}

int main() {
  int a = 1, b = 1, c = 1, d = 1;
  int result = foo(a,b,c,d); // all same, so foo returns 15
  if (result < 10) return 4; // 15 !< 10
  if (result > 20) return 5; // 15 !> 20
  return 6;                  // so return 6
} 
