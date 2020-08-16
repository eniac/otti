/* Rewrite the function lower, which converts upper case letters to lower case, 
   with a conditional expression instead of if-else. */

int lower(int c)
{
  return ((c <= 'Z') && (c >= 'A')) ? (c + 32) : c;
}

int main(void)
{
  char s[] = "lsk L";
  int i;
  return lower(s[4]); // 108
}
