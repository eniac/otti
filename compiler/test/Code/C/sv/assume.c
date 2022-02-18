extern void abort(void);
void reach_error(){}

extern unsigned int __VERIFIER_nondet_uint(void);
void __VERIFIER_assume(int cond);
void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: {reach_error();abort();}
  }
  return;
}
int main()
{
  unsigned int x,y;

  x=__VERIFIER_nondet_uint();
  y=__VERIFIER_nondet_uint();
  __VERIFIER_assume(x > 3);
  __VERIFIER_assume(y <= 4);
  __VERIFIER_assert(x != y);
  return 0;
}
