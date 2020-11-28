
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h>

​

#define T 30


typedef struct {
  int rows, cols; // mat[m x n]
  double mat[T];
} Tableau;

​

static const double epsilon   = 1.0e-10;
int d_equal(double a, double b) {
  return (fabs(a-b) < epsilon);
}

​

double get(int i, int j, Tableau tab){
  return tab.mat[i*(tab.cols)+j];
}

​

Tableau calculate_dual(Tableau p){
  //transpose
  double mat[p.cols*p.rows];
  Tableau transpose = {p.cols, p.rows, {0}};
  for (int i = 0; i < transpose.rows; i++) {
      for (int j = 0; j < transpose.cols; j++) {
        transpose.mat[i*(transpose.cols)+j] = get(j,i,p);
      }
  }
  return transpose;
}

​


Tableau simplex_max(Tableau tableau) {
  for(int i=1; i<tableau.rows; i++) {
    for(int j=1; j<tableau.rows; j++) {
      int change = tableau.cols - tableau.rows + 1;
      tableau.mat[i*(tableau.cols)+(j+change-1)] = (i==j);
    }
  }



  for (int i = 0; i < T; i++){
    printf("%f\n", tableau.mat[i]);

  }

​

  int pivot_col = 1;

​

  double lowest = get(0,pivot_col,tableau);

​

  for(int j=1; j<tableau.cols; j++) {

    double tab0j = get(0,j,tableau);

    if (tab0j < lowest) {

      lowest = tab0j;

      pivot_col = j;

    }

  }

​

  int pivot_row = 0;

  double min_ratio = -1;

​

  for(int i=1;i<tableau.rows;i++){

    double ratio = get(i,0,tableau) / get(i,pivot_col,tableau);

    if ( (ratio > 0  && ratio < min_ratio ) || min_ratio < 0 ) {

      min_ratio = ratio;

      pivot_row = i;

​

    }

  }

​

  while( !(lowest >= 0) && !(min_ratio == -1)) {

    int pivot_col, pivot_row;

​

    pivot_col = 1;

    lowest = get(0, pivot_col, tableau);

​

    for(int j=1; j<tableau.cols; j++) {

      double tab0j = get(0,j,tableau);

      if (tab0j < lowest) {

        lowest = tab0j;

        pivot_col = j;

      }

    }

​

    if( lowest >= 0 ) {

      break;

    }

​

    pivot_row = 0;

    min_ratio = -1;

​

    for(int i=1;i<tableau.rows;i++){

      double ratio = get(i,0,tableau) / get(i,pivot_col,tableau);

​

      if ((ratio > 0  && ratio < min_ratio ) || min_ratio < 0 ) {

        min_ratio = ratio;

        pivot_row = i;

​

      }

    }

​

    if (min_ratio == -1) {

      break;

    }

​

    double pivot = get(pivot_row, pivot_col, tableau);

​

    for(int j=0;j<tableau.cols;j++) {

      tableau.mat[pivot_row*(tableau.cols)+j] = get(pivot_row,j,tableau) / pivot;

    }

​

    for(int i=0; i<tableau.rows; i++) {

​

      double multiplier = get(i, pivot_col, tableau);

      if(i==pivot_row) {continue;}

      for(int j=0; j<tableau.cols; j++) {

        double set_val = get(i,j,tableau) - (multiplier * get(pivot_row,j,tableau));

        tableau.mat[i*(tableau.cols)+j] = set_val;

​

      }

    }

  }

​

  return tableau;

}

​

Tableau simplex_min(Tableau tableau) {

​

  for(int i=1; i<tableau.rows; i++) {

    for(int j=1; j<tableau.rows; j++) {

      int change = tableau.cols - tableau.rows + 1;

      tableau.mat[i*(tableau.cols)+(j+change-1)] = (i==j);

    }

  }

​

  for (int i = 0; i < T; i++){

    printf("%f\n", tableau.mat[i]);

  }

​

  int pivot_col = 1;

​

  double highest = get(0,pivot_col,tableau);

​

  for(int j=1; j<tableau.cols; j++) {

    double tab0j = get(0,j,tableau);

    if (tab0j > highest) {

      highest = tab0j;

      pivot_col = j;

    }

  }

​

  int pivot_row = 0;

  double min_ratio = -1;

​

  for(int i=1;i<tableau.rows;i++){

    double ratio = get(i,0,tableau) / get(i,pivot_col,tableau);

    if ( (ratio > 0  && ratio < min_ratio ) || min_ratio < 0 ) {

      min_ratio = ratio;

      pivot_row = i;

​

    }

  }

​

  while( !(highest <= 0) && !(min_ratio == -1)) {

    int pivot_col, pivot_row;

​

    pivot_col = 1;

    highest = get(0, pivot_col, tableau);

​

    for(int j=1; j<tableau.cols; j++) {

      double tab0j = get(0,j,tableau);

      if (tab0j > highest) {

        highest = tab0j;

        pivot_col = j;

      }

    }

​

    if( highest <= 0 ) {

      break;

    }

​

    pivot_row = 0;

    min_ratio = -1;

​

    for(int i=1;i<tableau.rows;i++){

      double ratio = get(i,0,tableau) / get(i,pivot_col,tableau);

​

      if ((ratio > 0  && ratio < min_ratio ) || min_ratio < 0 ) {

        min_ratio = ratio;

        pivot_row = i;

​

      }

    }

​

    if (min_ratio == -1) {

      break;

    }

​

    double pivot = get(pivot_row, pivot_col, tableau);

​

    for(int j=0;j<tableau.cols;j++) {

      tableau.mat[pivot_row*(tableau.cols)+j] = get(pivot_row,j,tableau) / pivot;

    }

​

    for(int i=0; i<tableau.rows; i++) {

​

      double multiplier = get(i, pivot_col, tableau);

      if(i==pivot_row) {continue;}

      for(int j=0; j<tableau.cols; j++) {

        double set_val = get(i,j,tableau) - (multiplier * get(pivot_row,j,tableau));

        tableau.mat[i*(tableau.cols)+j] = set_val;

​

      }

    }

  }

​

  return tableau;

}

​

Tableau simplex(int max_min, Tableau tableau){

  if (max_min==1) {

    return simplex_max(tableau);

  } else {

    return simplex_min(tableau);

  }

}

​

int satisfies(double sol[], Tableau tab){

  int sat = 1;

  double prod[tab.rows];



  for (int i=0;i<tab.rows;i++){

    prod[i]=0.0;

  }

​

  for (int i=0;i<tab.rows;i++){

    for (int j=1;j<tab.cols;j++){ //don't need 1st col

      prod[i] += (get(i,j,tab)*sol[j]);

    }

  }

​

  //eq?

  for (int i = 0; i < tab.rows; i++){

    sat = (d_equal(get(i,0,tab),prod[i]));

  }

​

​

  return sat;

​

}

​

​

double find_opt_var(Tableau t, int j){

  int x=-1;

  for(int i=1; i < t.rows; i++) {

    if (d_equal(get(i, j, t), 1)) {

      if (x == -1) {

        x=i;

      } else {

        return 0.0;

      }

    } else if (!d_equal(get(i,j,t),0)) {

      return 0.0;

    }

  }

  return get(x,0,t);

}

​

int solution_eq(double c[], double x[], int lenx, double y[], double b[], int leny){

  //cT * x = yT * b

  double xc = 0.0;

  for (int i = 0; i < lenx; i++){

    xc += c[i] * x[i];

  }

​

  double yb = 0.0;

  for (int i = 0; i < leny; i++){

    yb += y[i] * b[i];

  }

​

  return (xc == yb);

}

​

​

Tableau duality_gap_0(Tableau p_tableau, int p_max_min) {

  // calculate primal solution

  Tableau p_sol_tab = simplex(p_max_min, p_tableau);

  double x[p_sol_tab.cols+1]; // solution val at end



  for(int i=1; i<p_sol_tab.cols; i++) {

    x[i] = find_opt_var(p_sol_tab, i);

  }

  x[p_sol_tab.cols] = p_sol_tab.mat[0];

​

​

  // calculate dual solution

  Tableau d_tab = calculate_dual(p_tableau);

​

  int d_max_min = !p_max_min;

  Tableau d_sol_tab = simplex(d_max_min, d_tab);

  double y[d_sol_tab.cols+1];



  for(int i=1; i<d_sol_tab.cols; i++) {

    y[i] = find_opt_var(d_sol_tab, i);

  }

  y[d_sol_tab.cols] = d_sol_tab.mat[0];

​

  //calculate lower cost solution

​

  double c[p_sol_tab.rows - 1];

  for (int i = 1; i < p_sol_tab.rows; i++){

    c[i-1] = get(i,0,p_sol_tab);

  }

​

  double b[d_sol_tab.rows - 1];

  for (int i = 1; i < d_sol_tab.rows; i++){

    b[i-1] = get(i,0,d_sol_tab);

  }

​

  return p_sol_tab;

​

  //__VERIFY_assert(solution_eq(c, x, (p_sol_tab.rows - 1) y, b, (d_sol_tab.rows - 1)) && (satisfies(primal, p_tab)));

}

​

​

​

int main(void) {

  printf("Hello World\n");

  //tableau.cols += tableau.rows -1; - have to artifically add in slack rows here - edit later

​

  Tableau tab  = { 4, 7,

      { 0.0 , -1.0 , 3.0 ,-3.0, 0.0, 0.0, 0.0,

        7.0 ,  3.0 ,  -1.0 , 2.0, 0.0, 0.0, 0.0,

        12.0 , -2.0 , -4.0 , 0.0, 0.0, 0.0, 0.0,

        10.0 ,  -4.0 ,  3.0 , 8.0, 0.0, 0.0, 0.0

        } };

​

Tableau p_sol_tab = simplex(1, tab);

double p_opt[p_sol_tab.cols];



for (int i = 0; i < T; i++){

    printf("%f\n", p_sol_tab.mat[i]);

}

​

printf("\n\n\n");

  for(int i=1; i<p_sol_tab.cols; i++) {

    p_opt[i] = find_opt_var(p_sol_tab, i);

    printf("%f\n", p_opt[i]);

  }

​

printf("SAT%d\n", satisfies(p_opt, p_sol_tab));

​

}
