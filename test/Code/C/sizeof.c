int sizeof_expr() {
  int x = 0;
  return sizeof(x);
}

int sizeof_expr_no_eval() {
  int x = 0;
  sizeof(x = 1);
  return x;
}

int sizeof_type() {
  return sizeof(int);
}
