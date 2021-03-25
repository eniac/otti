

  if (max_min){ //make dual max
    for (int i = 0; i < VP; i++) {
        for (int j = 0; j < CP; j++) {
          if (i == 0) {
            transpose.mat[i*(transpose.cols)+j] = get(j,i,p) * -1;
          } else {
            transpose.mat[i*(transpose.cols)+j] = get(j,i,p);
          }

        }
    }


  } else { //make dual min
    for (int i = 0; i < VP; i++) {
        for (int j = 0; j < CP; j++) {
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


Tableau maximize(Constraint c){
  Tableau t = make_problem();

  t.mat[0] = c.goal;
  for (int i = 1; i < CP; i++){
    t.mat[i] = c.vars[i-1] * -1;
  }

  return t;
}

Tableau minimize(Constraint c){
  Tableau t = make_problem();

  t.mat[0] = c.goal;
  for (int i = 1; i < CP; i++){
    t.mat[i] = c.vars[i-1];
  }

  return t;
}

Tableau add(Tableau t, Constraint c){

  t.mat[t.cntr*t.cols] = c.goal;
  for (int j = 1; j < VP; j++){
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

    if( lowest < 0 ) {
      pivot_row = 0;
      no_ratio = 1;
      fixed_point_precision_16_16 min_ratio = 0;

      for(int i=1;i<tableau.rows;i++){
        fixed_point_precision_16_16 entry = get(i,0,tableau);
        fixed_point_precision_16_16 pot_pivot = get(i,pivot_col,tableau);
        if (pot_pivot > 0 && !(d_equal(pot_pivot,0))) {
          fixed_point_precision_16_16 ratio = entry / pot_pivot; //Issue with prover here

          if (ratio < min_ratio || no_ratio == 1) {
            min_ratio = ratio;
            pivot_row = i;
            no_ratio = 0;
          }

        }

      }

      if (no_ratio == 0) {
        fixed_point_precision_16_16 pivot = get(pivot_row, pivot_col, tableau);


        for(int k=0;k<tableau.cols;k++) {
          if (!(d_equal(pivot,0.0))){ //ADDED FOR PROVER
            tableau.mat[pivot_row*(tableau.cols)+k] = get(pivot_row,k,tableau) / pivot;
          }

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
    int b = 1;

    for (int r=1; r < tableau.rows; r++){
      if (tableau.stars[r] && b) {

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

        b = 0;
      }
    }


    if( highest > 0 ) { //?

      pivot_row = 0;

      int no_ratio = 1;
      fixed_point_precision_16_16 min_ratio = 0;

      for(int i=1;i<tableau.rows;i++){
        fixed_point_precision_16_16 entry = get(i,0,tableau);
        fixed_point_precision_16_16 pot_pivot = get(i,pivot_col,tableau);
        if (pot_pivot > 0 && !(d_equal(pot_pivot,0))) {
          fixed_point_precision_16_16 ratio = entry / pot_pivot; //issue here

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
          if (!(d_equal(pivot,0))){ //ADDED FOR PROVER
            tableau.mat[pivot_row*(tableau.cols)+k] = get(pivot_row,k,tableau) / pivot;
          }
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
    //printf("too many iterations, problem unsolvable, check constraints");
  }


  return tableau;
}


fixed_point_precision_16_16 find_opt_var(Tableau t, int j){
  fixed_point_precision_16_16 zero = 0;
  int x=-1;
  for(int i=1; i < t.rows; i++) {
    if (d_equal(get(i, j, t), 1)) {
      if (x == -1) {
        x=i;
      } else {
        return zero;
      }
    } else if (!d_equal(get(i,j,t),0)) {
      return zero;
    }
  }
  return get(x,0,t);
}



Solution_Box simplex_gadget(Tableau p_tableau, int p_max_min) {

  Solution_Box sol = __GADGET_compute(simplex_prover(p_tableau, p_max_min));
  //printf("CHECK %d", simplex_check(p_max_min, sol));
  __GADGET_check(simplex_check(p_max_min, sol));

  return sol;
}
