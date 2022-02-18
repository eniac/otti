

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

int baz () {

  int x = 20;
  if (x > 10) x = 10;
  if (x > 20) x = 20;
  if (x > 30) x = 30;
  if (x > 40) x = 40;
  return x;
  
}

int qux () {

  int x = 20;
  int y = 30;
  
  if (x < 15) x = 500; // 20 > 15, take the false. x = 601
  else x = 601; 
  
  if (y > 15) y = 5; // 30 > 15, take the true. y = 5
  else y = 1; 

  if (x % 2 == 0) return 20; // 601 is not divisible by 2...
  else return x + y; // ...return 601+5 = 606
  
}
