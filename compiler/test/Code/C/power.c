/* Test for a power function. */

int power(int m, int n);

int main(void)
{
  return power(2,3);
}

/* power: raise base to n-th power; n >= 0 */
int power(int base, int n)
{
  int i, p;

  p = 1;
  for (i = 1; i <= n; ++i)
    p = p * base;
  return p;
}
