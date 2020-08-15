

int foo () {

  int x = 4;
  
  { int x = 5; }
  
  return x;
}

int bar () {

  int x = 4;
  if (x < 5) { int x = 3; }
  else { int x = 4; }
  
  return x;
}
