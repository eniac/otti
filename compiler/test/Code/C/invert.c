/* Write a function invert(x,p,n) that returns x with the n bits that begin at position
   p inverted (i.e. 1 changed into 0 and vice versa, leaving the others unchanged. */


int invert(int x, int p, int n)
{
  int ref = 0;

  ref = ~ref << n;
  ref = ~ref << p;  // mark the exact bits that need negation

  return ((~ref & x) | (ref & ~x));
}

int main(void)
{
  int x, p, n;
  x = 123475;
  p = 4;
  n = 5; 

  return invert(x, p, n);
}
