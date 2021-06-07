typedef float fp32;

#define epsilon (fp32)1.0e-2

int d_equal(fp32 a, fp32 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b) < epsilon;
  }
}


int test_check(fp32 x0){
  int solved = (d_equal(x0,1.706));
  return solved;
}

int test(){

  fp32 x0 = 3;// = __GADGET_exist();
  
  x0 = (fp32)1.706;

  int check = __GADGET_check(test_check(x0));


  return check;
}
