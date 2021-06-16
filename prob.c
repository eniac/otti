
int comp(int a){
  int r = 10 - a;
  return r;
}

int check(int ex){
  return (ex == 7);
}


void test(){

  int a = 3;

  int r = __GADGET_compute(comp(a));

  __GADGET_check(r);


}




