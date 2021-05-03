int main() {
  return __LINEAR_maximize(
      3 * x + 4 * y,   // objective
      x + 2 * y <= 14, // constraint 1
      3 * x - y >= 0,  // constraint 2
      x - y <= 2);     // constraint 3
}

