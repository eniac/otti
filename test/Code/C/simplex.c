#include <stdio.h>
#include "simplex.h"

//API
Tableau make_problem(){
  Tableau tab = { C+1, V+C+1, {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, {0,0,0}, 1 };

  return tab;
}

Tableau maximize(Tableau t, Constraint c){
  t.mat[0] = c.goal;
  for (int i = 1; i < C+1; i++){
    t.mat[i] = c.vars[i-1] * -1;
  }

  return t;
}

Tableau minimize(Tableau t, Constraint c){
  t.mat[0] = c.goal;
  for (int i = 1; i < C+1; i++){
    t.mat[i] = c.vars[i-1];
  }

  return t;
}

Tableau add(Tableau t, Constraint c){

  t.mat[t.cntr*t.cols] = c.goal;
  for (int j = 1; j < V+1; j++){
    t.mat[(t.cntr*t.cols)+j] = c.vars[j-1];
  }

  t.cntr = t.cntr + 1;
  return t;


}


//simplex
int d_equal(fixed_point_precision_16_16 a, fixed_point_precision_16_16 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b) < epsilon;
  }
}

fixed_point_precision_16_16 get(int i, int j, Tableau tab){
  return tab.mat[i*(tab.cols)+j];
}

void print_sol(Solution s){
  printf("\nSolution at:\n");
  for (int i = 0; i < V; i++){
    if (d_equal((int)(s.x[i]), (s.x[i]))) {
        printf("x%d: %6d\n", (i+1), (int)s.x[i]);
      } else {
        printf("x%d: %6.3f\n", (i+1), s.x[i]);
      }
  }
  printf("val: %6.3f\n", s.x[V]);
}

void print_tab(Tableau tab) {

  printf("\nTableau:\n-----------------\n");

  printf("%-6s%5s", "var:", "curr");
  for(int j=1;j<tab.cols; j++) { printf("    x%d,", j); }
  printf("\n");

  for(int i=0;i<tab.rows; i++) {
    if (i==0) {
      printf("max:");
    } else {
      printf("b%d: ", i);
    }
    for(int j=0;j<tab.cols; j++) {
      if (d_equal((int)(get(i,j,tab)), (get(i,j,tab)))) {
        printf(" %6d", (int)get(i,j,tab));
      } else {
        printf(" %6.3f", get(i,j,tab));
      }
    }
    printf("\n");

  }
  printf("stars: ");
  for(int j=0;j<tab.rows; j++) {
    printf("  %d", tab.stars[j]);
  }



  printf("\n----------------\n");
}

Tableau add_slack(Tableau tab, int max_min, int vars){

  int slack = tab.rows - 1;

  int index = 0;
  for (int i=1; i<tab.rows; i++) {

      for (int j=vars+1; j<vars+1+slack; j++) {
        if (j - vars == i) {
          if (max_min) {
            tab.mat[i*(tab.cols)+j] = 1.0;
          } else {
            tab.mat[i*(tab.cols)+j] = -1.0;
            tab.stars[i] = 1;
          }
        }

      }

  }

  return tab;

}


Tableau calculate_dual(Tableau p, int max_min){
  //transpose
  Tableau transpose = {V+1, C+V+1, {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, {0,0,0}, 1 };

  if (max_min){ //make dual max
    for (int i = 0; i < V+1; i++) {
        for (int j = 0; j < C+1; j++) {
          if (i == 0) {
            transpose.mat[i*(transpose.cols)+j] = get(j,i,p) * -1;
          } else {
            transpose.mat[i*(transpose.cols)+j] = get(j,i,p);
          }

        }
    }


  } else { //make dual min
    for (int i = 0; i < V+1; i++) {
        for (int j = 0; j < C+1; j++) {
          if (j == 0) {
            transpose.mat[i*(transpose.cols)+j] = get(j,i,p) * -1;
          } else {
            transpose.mat[i*(transpose.cols)+j] = get(j,i,p);
          }

        }
    }


  }



  return transpose;
}


Tableau simplex_max(Tableau tableau) {

  fixed_point_precision_16_16 lowest = -1;
  int no_ratio = 0;

  while( !(lowest >= 0) && no_ratio == 0) {
    int pivot_col, pivot_row;

    pivot_col = 1;
    lowest = get(0, pivot_col, tableau);

    int slack = tableau.rows - 1;
    for(int j=1; j<tableau.cols-slack; j++) {

      fixed_point_precision_16_16 tab0j = get(0,j,tableau);
      if (tab0j < lowest) {
        lowest = tab0j;
        pivot_col = j;
      }

    }

    if( lowest >= 0 ) {
      break;
    }

    pivot_row = 0;
    no_ratio = 1;
    fixed_point_precision_16_16 min_ratio = 0;

    for(int i=1;i<tableau.rows;i++){
      fixed_point_precision_16_16 entry = get(i,0,tableau);
      fixed_point_precision_16_16 pot_pivot = get(i,pivot_col,tableau);
      if (pot_pivot > 0) {
        fixed_point_precision_16_16 ratio = entry / pot_pivot;

        if (ratio < min_ratio || no_ratio == 1) {
          min_ratio = ratio;
          pivot_row = i;
          no_ratio = 0;
        }

      }

    }

    if (no_ratio) {
      break;
    }

    fixed_point_precision_16_16 pivot = get(pivot_row, pivot_col, tableau);


    for(int k=0;k<tableau.cols;k++) {
      tableau.mat[pivot_row*(tableau.cols)+k] = get(pivot_row,k,tableau) / pivot;
    }

    for(int l=0; l<tableau.rows; l++) {

      fixed_point_precision_16_16 multiplier = get(l, pivot_col, tableau);
      if(l!=pivot_row) {
        for(int m=0; m<tableau.cols; m++) {
          fixed_point_precision_16_16 set_val = get(l,m,tableau) - (multiplier * get(pivot_row,m,tableau));
          tableau.mat[l*(tableau.cols)+m] = set_val;
        }
      }
    }


  }

  return tableau;
}

Tableau simplex_stars(Tableau tableau) {

  fixed_point_precision_16_16 highest = 0;
  int no_ratio = 0;
  _Bool stars = 1;

  int loop = 0;
  while(stars && loop < 20) {
  loop = loop + 1;

    int pivot_col, pivot_row;

    for (int r=1; r < tableau.rows; r++){
      if (tableau.stars[r]) {

        pivot_col = 1;
        highest = get(r,pivot_col,tableau); // what if no positives?

        int slack = tableau.rows - 1;
        for(int j=1; j<tableau.cols; j++) {

          fixed_point_precision_16_16 tabrj = get(r,j,tableau);

          if (tabrj > highest) {
            highest = tabrj;
            pivot_col = j;

          }
        }

        break;
      }
    }


    if( highest > 0 ) { //?

      pivot_row = 0;

      int no_ratio = 1;
      fixed_point_precision_16_16 min_ratio = 0;

      for(int i=1;i<tableau.rows;i++){
        fixed_point_precision_16_16 entry = get(i,0,tableau);
        fixed_point_precision_16_16 pot_pivot = get(i,pivot_col,tableau);
        if (pot_pivot > 0) {
          fixed_point_precision_16_16 ratio = entry / pot_pivot;

          if (ratio == min_ratio && tableau.stars[i]) { // test for no ratio?
            min_ratio = ratio;
            pivot_row = i;
            no_ratio = 0;
          }

          if (ratio < min_ratio || no_ratio == 1) {
            min_ratio = ratio;
            pivot_row = i;
            no_ratio = 0;
          }

        }

      }

      if (!no_ratio) {
        fixed_point_precision_16_16 pivot = get(pivot_row, pivot_col, tableau);


        tableau.stars[pivot_row] = 0;

        for(int k=0;k<tableau.cols;k++) {
          tableau.mat[pivot_row*(tableau.cols)+k] = get(pivot_row,k,tableau) / pivot;
        }

        for(int l=0; l<tableau.rows; l++) {

          fixed_point_precision_16_16 multiplier = get(l, pivot_col, tableau);
          if(l!=pivot_row) {
            for(int m=0; m<tableau.cols; m++) {
              fixed_point_precision_16_16 set_val = get(l,m,tableau) - (multiplier * get(pivot_row,m,tableau));
              tableau.mat[l*(tableau.cols)+m] = set_val;
            }
          }
        }
      } // end no ratio

    } // end highest

  stars = 0;
  for (int s=0; s<tableau.rows; s++) {
    stars = stars || tableau.stars[s];

  }

  }

  if (loop == 20){
    printf("too many iterations, problem unsolvable, check constraints");
  }


  return tableau;
}


int satisfies(fixed_point_precision_16_16 sol[], int sol_len, Tableau tab, int max_min){
  int sat = 1;
  fixed_point_precision_16_16 prod[tab.rows];

  for (int a=0;a<tab.rows;a++){
    prod[a]=0.0;
  }

  for (int i=0;i<tab.rows;i++){
    for (int j=0;j<sol_len;j++){
      prod[i] += (get(i,(j+1),tab)*sol[j]); //don't need 1st col
    }
  }
  //eq?
  if (max_min){ // max prob
     for (int k = 1; k < tab.rows; k++){
        sat = sat && (d_equal(get(k,0,tab),prod[k]) || get(k,0,tab) > prod[k]);
      }
  } else { // min prob
     for (int l = 1; l < tab.rows; l++){
        sat = sat && (d_equal(get(l,0,tab),prod[l]) || get(l,0,tab) < prod[l]);
      }
  }

  return sat;

}


fixed_point_precision_16_16 find_opt_var(Tableau t, int j){
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

int solution_eq(fixed_point_precision_16_16 c[], fixed_point_precision_16_16 x[], int lenx, fixed_point_precision_16_16 y[], fixed_point_precision_16_16 b[], int leny, int max_min){

  //cT * x = yT * b
  fixed_point_precision_16_16 xc = 0.0;
  for (int i = 0; i < lenx; i++){
    xc += c[i] * x[i];

  }
  printf("XC: %f\n", xc);

  fixed_point_precision_16_16 yb = 0.0;
  for (int i = 0; i < leny; i++){
    yb += y[i] * b[i];
  }

  printf("YB: %f\n", yb);

  if (max_min){
    return (xc + yb == 0);
  } else {
    return (xc == yb);
  }


}


Solution simplex_prover(Tableau p_tableau, int p_max_min) {
  //PROVER CODE
  // calculate primal solution
  print_tab(p_tableau);
  Tableau p_sol_tab_a = add_slack(p_tableau, p_max_min, V);
  print_tab(p_sol_tab_a);

  Tableau p_sol_tab_b = simplex_stars(p_sol_tab_a);
  Tableau p_sol_tab = simplex_max(p_sol_tab_b);

  print_tab(p_sol_tab);

  int d_max_min = !p_max_min;
  Tableau d_tableau = calculate_dual(p_tableau, d_max_min);

  Tableau d_sol_tab_a = add_slack(d_tableau, d_max_min, C);
  Tableau d_sol_tab_b = simplex_stars(d_sol_tab_a);
  Tableau d_sol_tab = simplex_max(d_sol_tab_b);

  printf("reach");

  Solution sol = {{0.0,0.0,0.0}, {0.0,0.0,0.0,0.0}};


  for(int i=0; i<V; i++) {
    sol.x[i] = find_opt_var(p_sol_tab, (i+1));
  }
  sol.x[V] = p_sol_tab.mat[0];

  fixed_point_precision_16_16 y[C+1];

  for(int i=0; i<C; i++) {
    sol.y[i] = find_opt_var(d_sol_tab, (i+1));

  }
  sol.y[C] = d_sol_tab.mat[0];



  return sol;

}

int simplex_check(fixed_point_precision_16_16 c[], fixed_point_precision_16_16 x[], int lenx, fixed_point_precision_16_16 y[], fixed_point_precision_16_16 b[], int leny, int max_min, Tableau p_tableau) {
    return solution_eq(c, x, lenx, y, b, leny, max_min) && satisfies(x, lenx, p_tableau, max_min);
}


Solution simplex_gadget(Tableau p_tableau, int p_max_min) {
  // prover
  //Solution sol = __GADGET_compute(simplex_prover(p_tableau, p_max_min));
  Solution sol = simplex_prover(p_tableau, p_max_min);



  //b and c for solution equality
  fixed_point_precision_16_16 c[V];
  for (int i = 1; i < V+1; i++){
    c[i-1] = get(0,i,p_tableau);
  }

  fixed_point_precision_16_16 b[C];
  for (int i = 1; i < C+1; i++){
    b[i-1] = get(i,0,p_tableau);
  }



  printf("CHECK %d", simplex_check(c, sol.x, V, sol.y, b, C, p_max_min, p_tableau));

  //verifier - c, x, V, y, b, C
  //__GADGET_rewrite(simplex_check(p_max_min, p_tableau));

  return sol;
}
