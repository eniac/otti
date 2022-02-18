/* Write a function setbits(s,p,n,y) that returns x with the n bits that begin at
   position p set to the rightmost n bits of y, leaving the other bits unchanged. */

int setbits(int x, int p, int n, int y);

int main(void)
{
  int x, p, n, y;

  x = 1234;
  y = 567;
  p = 8;
  n = 9;
  return setbits(x, p, n, y);
}

int setbits(int x, int p, int n, int y)
{
  y = y >> (p-1);      // move the bits starting at pos p to the end
  y = y & ~(~0 << n);  // null all the bits from a pos higher than n
  x = x & (~0 << n);   // null the last n bits
  return (x | y);
}
