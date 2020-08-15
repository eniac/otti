

int foo () {

  int x = 5;
  if (x > 10) return 1;
  if (x > 20) return 2;
  if (x > 30) return 3;
  if (x > 40) return 4;
  return 5;
  
}

int bar () {

  int x = 5;
  if (x < 10) x = 10;
  if (x < 20) x = 20;
  if (x < 30) x = 30;
  if (x < 40) x = 40;
  return x;
  
}
