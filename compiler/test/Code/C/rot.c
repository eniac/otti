/* Write a function rightrot(x,n) that returns the value of the integer x rotated
   to the right by n bit positions. */

int rightrot(int x, int n)
{
  int end = 0;

  end = ~(~end << n);
  end = x & end;
  end = end << (8 * sizeof(x) - n);
  // This is wrong? b/c sign extension?
  return ((x >> n) | end);
}


int main(void)
{
  int x, n;
  x = 4;
  n = 4;
  return rightrot(x, n); 
}

int other_rightrot(int x, int n)
{
  int end = 0;
  return (x << (8 * sizeof(x) - n)) | (x >> n);
}

int other_main()
{
  int x, n;
  x = 4;
  n = 4;
  return other_rightrot(x, n);
}
