
int add() {

  typedef signed int fixed_point_precision;

  fixed_point_precision x = 4294967296; // 1.0

  fixed_point_precision y = 2147483648; // 0.5


  return (x+y); // 1.5
}

int sub() {

  typedef signed int fixed_point_precision;

  fixed_point_precision x = 8589934592; // 2.0

  fixed_point_precision y = 2147483648; // 0.5


  return (x-y); // 1.5
}
