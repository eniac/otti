


typedef struct {
  int rows, cols; // mat[m x n]
  int mat[18];
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

int find_opt_var(Tableau t, int j){
  int x=-1;
  for(int i=1; i < t.rows; i++) {
    if (d_equal(get(i, j, t), 1)) {
      if (x == -1) {
        x=i;
      } else {
        return 0;
      }
    } else if (!d_equal(get(i,j,t),0)) {
      return 0;
    }
  }
  return get(x,0,t);
}

int main(void) {

  Tableau tab  =  { 3, 4,
        { 0 , -6*(2^16) , -14*(2^16) , -13*(2^16), 0, 0,
       24*(2^16) ,  32768 ,  2*(2^16), 1*(2^16), 0, 0,
       60*(2^16) , 1*(2^16) , 2*(2^16), 4*(2^16), 0, 0  }
    };

   for(int i=1; i<tab.rows; i++) {
     for(int j=1; j<tab.rows; j++) {
       int change = tab.cols - tab.rows + 1;
       tab.mat[i*(tab.cols)+(j+change-1)] = (i == j);
     }
   }

   int lowest = -1*(2^16);
   int min_ratio = 0;

   while( !(lowest >= 0) && !(min_ratio == -1)) {
     int pivot_col, pivot_row;

     pivot_col = 1;
     lowest = get(0, pivot_col, tab);

     int slack = tab.rows - 1;
     for(int j=1; j<tab.cols-slack; j++) {

       int tab0j = get(0,j,tab);
       if (tab0j < lowest) {
         lowest = tab0j;
         pivot_col = j;
       }

     }


     if( lowest >= 0 ) {
       break;
     }

     pivot_row = 0;
     min_ratio = -1;

     for(int i=1;i<tab.rows;i++){
       int ratio = (get(i,0,tab) / get(i,pivot_col,tab)) * (2^16);

       if ((ratio > 0  && ratio < min_ratio ) || min_ratio < 0 ) {
         min_ratio = ratio;
         pivot_row = i;

       }
     }

     if (min_ratio == -1) {
       break;
     }

     int pivot = get(pivot_row, pivot_col, tab);

     for(int jj=0;jj<tab.cols;jj++) {
       tab.mat[pivot_row*(tab.cols)+jj] = (get(pivot_row,jj,tab) / pivot) * (2^16);
     }

     for(int ii=0; ii<tab.rows; ii++) {

       int multiplier = get(ii, pivot_col, tab);
       if(ii!=pivot_row) {
         for(int k=0; k<tab.cols; k++) {
           int multed = (multiplier * get(pivot_row,k,tab)) / (2^16);

           int set_val = get(ii,k,tab) - multed;
           tab.mat[ii*(tab.cols)+k] = set_val;
         }
       }

     }
  }

   int x[5] = {0,0,0,0,0};

   for(int l=1; l<tab.cols; l++) {
     x[i] = find_opt_var(tab, l);
   }
   x[4] = tab.mat[0];



   int correct[5]  =  {36*(2^16),0,6*(2^16),0,294*(2^16)};

   int output = 0;
   for (int y = 0; y<18; y++){
     if (correct[y] == x[y]){
      output = 1;
     }
   }

   return (output);

}
