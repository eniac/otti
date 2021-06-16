#include <stdlib.h>
#include <stdio.h>

#define N 3

void plus_ten(int a, int n) {
  a = n + 10;
}

void plus_all(int len, int *arra, int *arrb){

  for (int i = 0; i < len; i++){
    arra[i] = arrb[i];
  }

}

/*
int main(){
  int arr[3] = {2,2,2};

  int *new = plus_all(3, arr);

  for (int i = 0; i < 3; i++){
    printf("%d\n", new[i]);
  }

}
*/

