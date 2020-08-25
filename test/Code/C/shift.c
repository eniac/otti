

int lneg (int x) {
  return x << -1; 
}

int rneg (int x) {
  return x >> -1; 
}

int lover (int x) {
  return x << 32; 
}

int rover (int x) {
  return x >> 32; 
}

int signoff (int x) {
  return -1 << x;
}

int arith (int x) {
  return -1 >> x;
}

int any (int x, int y) {
  return x << y;
}

int usany (unsigned int x, unsigned int y) {
  return x << y;
}
