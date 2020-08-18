float fahrenheit(int c);

int main(void)
{
  return fahrenheit(140);
}

float fahrenheit(int c)
{
  return ((5.0/9.0)*(c-32));
}
