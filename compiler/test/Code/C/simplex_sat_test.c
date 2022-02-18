typedef struct {
  int rows, cols; // mat[m x n]
  int mat[12];
} Tableau;

int get(int i, int j, Tableau tab){
  return tab.mat[i*(tab.cols)+j];
}

int d_equal(int a, int b) {
  if ((a-b) < 0){
    int r = ((a-b) > -2);
    return r;
  } else {
    int r = ((a-b) < 2);
    return r;
  }

}

int main(void) {

  Tableau p_sol_tab = { 3, 4,
        { 0 , -6*(2^16) , -14*(2^16) , -13*(2^16),
       24*(2^16) ,  32768 ,  2*(2^16), 1*(2^16),
       60*(2^16) , 1*(2^16) , 2*(2^16), 4*(2^16)  }
    };


  int x[5] = {36*(2^16),0,6*(2^16),0,294*(2^16)};

  int y[2] = {11*(2^16), 32768};


  //b and c for solution equality
  int c[5] = {6*(2^16), 14*(2^16), 13*(2^16), 0, 0};

  int dl = p_sol_tab.cols - 1;
  int b[2] = {24*(2^16), 60*(2^16)};


  int xc = 0;
  for (int i = 0; i < 5; i++){
    xc = xc + ((c[i] * x[i]) / (2^16));
  }

  int yb = 0;
  for (int j = 0; j < 2; j++){
    yb = yb + ((y[j] * b[j]) / (2^16));
  }


  int solution_eq = (d_equal(xc,yb));




  //sat
  int sat = 1;
  Tableau pos_tab = { 3, 4,
        { 0 , 6*(2^16) , 14*(2^16) , 13*(2^16),
       24*(2^16) ,  32768 ,  2*(2^16), 1*(2^16),
       60*(2^16) , 1*(2^16) , 2*(2^16), 4*(2^16)  }
    };;

  int prod[3] = {0,0,0};

  for (int u=0;u<pos_tab.rows;u++){
    for (int v=1;v<pos_tab.cols;v++){ //don't need 1st col
      prod[u] = prod[u] + (get(u,v,pos_tab)*x[v]);
    }
  }


  //eq?
  for (int w = 0; w < 3; w++){
    int d = d_equal(get(w,0,pos_tab),prod[w]);
    if (d == 0){
      sat = 0;
      break;
    }
  }



  int r = solution_eq && sat;

  return r;
}
