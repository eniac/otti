/* Rewrite the function lower, which converts upper case letters to lower case, 
   with a conditional expression instead of if-else. */

int lower(int c)
{
  return ((c <= 'Z') && (c >= 'A')) ? (c + 32) : c;
}

int main(void)
{
  char s[6] = "lsk L";
  int i = s[4];
  return lower(i); // 108
}
