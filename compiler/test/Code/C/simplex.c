#include <stdio.h>
#include "simplex.h"


int main(void) {

  Constraint obj = {{24.0, 60.0, 0, 0, 0}, LEQ, 0};
  Tableau tab = minimize(obj);

  Constraint c1 = {{0.5, 1.0, 0, 0, 0}, GEQ, 6.0};
  Constraint c2 = {{2.0, 2.0, 0, 0, 0}, GEQ, 14.0};
  Constraint c3 = {{1.0, 4.0, 0, 0, 0}, GEQ, 13.0};
  tab = add(tab, c1);
  tab = add(tab, c2);
  tab = add(tab, c3);

//  Solution_Box sol = simplex_prover(tab, MIN);
//  printf("CHECK %d", simplex_check(MIN, sol));

Solution_Box sol = simplex_gadget(tab, MIN);

  return 0;

}























//API
Tableau make_problem(){
  Tableau tab = { CP, V+C+1, {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, {0,0,0,0,0,0}, 1 };

  return tab;
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


Tableau calculate_dual(Tableau p, int max_min){
  //transpose
  Tableau transpose = {VP, C+V+1, {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, {0,0,0,0,0,0}, 1 };

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




Solution_Box simplex_prover(Tableau p_tableau, int p_max_min) {
  //PROVER CODE
  // calculate primal solution
  //print_tab(p_tableau);
  Tableau p_sol_tab_a = add_slack(p_tableau, p_max_min, V);

  Tableau p_sol_tab_b = simplex_stars(p_sol_tab_a);
  Tableau p_sol_tab = simplex_max(p_sol_tab_b);


  int d_max_min = !p_max_min;
  Tableau d_tableau = calculate_dual(p_tableau, d_max_min);

  Tableau d_sol_tab_a = add_slack(d_tableau, d_max_min, C);
  Tableau d_sol_tab_b = simplex_stars(d_sol_tab_a);
  Tableau d_sol_tab = simplex_max(d_sol_tab_b);


  Solution sol = {{0,0,0,0,0,0}, {0,0,0,0,0,0}};

  for(int i=0; i<V; i++) {
    sol.x[i] = find_opt_var(p_sol_tab, (i+1));
  }
  sol.x[V] = p_sol_tab.mat[0];
  fixed_point_precision_16_16 y[CP];
  for(int j=0; j<C; j++) {
    sol.y[j] = find_opt_var(d_sol_tab, (j+1));
  }
  sol.y[C] = d_sol_tab.mat[0];



  Solution_Box solf = { p_tableau.mat[0],
                      p_tableau.mat[1],
                      p_tableau.mat[2],
                      p_tableau.mat[3],
                      p_tableau.mat[4],
                      p_tableau.mat[5],
                      p_tableau.mat[6],
                      p_tableau.mat[7],
                      p_tableau.mat[8],
                      p_tableau.mat[9],
                      p_tableau.mat[10],
                      p_tableau.mat[11],
                      p_tableau.mat[12],
                      p_tableau.mat[13],
                      p_tableau.mat[14],
                      p_tableau.mat[15],
                      p_tableau.mat[16],
                      p_tableau.mat[17],
                      p_tableau.mat[18],
                      p_tableau.mat[19],
                      p_tableau.mat[20],
                      p_tableau.mat[21],
                      p_tableau.mat[22],
                      p_tableau.mat[23],
                      p_tableau.mat[24],
                      p_tableau.mat[25],
                      p_tableau.mat[26],
                      p_tableau.mat[27],
                      p_tableau.mat[28],
                      p_tableau.mat[29],
                      p_tableau.mat[30],
                      p_tableau.mat[31],
                      p_tableau.mat[32],
                      p_tableau.mat[33],
                      p_tableau.mat[34],
                      p_tableau.mat[35],
                      p_tableau.mat[36],
                      p_tableau.mat[37],
                      p_tableau.mat[38],
                      p_tableau.mat[39],
                      p_tableau.mat[40],
                      p_tableau.mat[41],
                      p_tableau.mat[42],
                      p_tableau.mat[43],
                      p_tableau.mat[44],
                      p_tableau.mat[45],
                      p_tableau.mat[46],
                      p_tableau.mat[47],
                      p_tableau.mat[48],
                      p_tableau.mat[49],
                      p_tableau.mat[50],
                      p_tableau.mat[51],
                      p_tableau.mat[52],
                      p_tableau.mat[53],
                      p_tableau.mat[54],
                      p_tableau.mat[55],
                      p_tableau.mat[56],
                      p_tableau.mat[57],
                      p_tableau.mat[58],
                      p_tableau.mat[59],
                      p_tableau.mat[60],
                      p_tableau.mat[61],
                      p_tableau.mat[62],
                      p_tableau.mat[63],
                      p_tableau.mat[64],
                      p_tableau.mat[65],
                      p_tableau.mat[66],
                      p_tableau.mat[67],
                      p_tableau.mat[68],
                      p_tableau.mat[69],
                      p_tableau.mat[70],

                      sol.x[0],
                      sol.x[1],
                      sol.x[2],
                      sol.x[3],
                      sol.x[4],
                      sol.x[5],

                      sol.y[0],
                      sol.y[1],
                      sol.y[2],
                      sol.y[3],
                      sol.y[4],
                      sol.y[5]

                    };


  return solf;

}

int simplex_check(int max_min, Solution_Box sf) {

    /*
    //find b and c
    fixed_point_precision_16_16 c[V];
    for (int i = 1; i < VP; i++){
      c[i-1] = get(0,i,p_tableau);
    }
    fixed_point_precision_16_16 b[C];
    for (int j = 1; j < CP; j++){
      b[j-1] = get(j,0,p_tableau);
    }
    */

    //SOLUTION EQUALITY
    int sol_eq = 0;

    //cT * x = yT * b
    //fixed_point_precision_16_16 xc = 0;
    fixed_point_precision_16_16 xc = (sf.m1 * sf.x0) +
                                     (sf.m2 * sf.x1) +
                                     (sf.m3 * sf.x2) +
                                     (sf.m4 * sf.x3) +
                                     (sf.m5 * sf.x4);
    /*
    for (int k = 0; k < V; k++){
      xc += c[k] * sol.x[k];
    } */

    fixed_point_precision_16_16 yb = (sf.m11 * sf.y0) +
                                     (sf.m22 * sf.y1) +
                                     (sf.m33 * sf.y2) +
                                     (sf.m44 * sf.y3) +
                                     (sf.m55 * sf.y4);
    /*
    fixed_point_precision_16_16 yb = 0.0;
    for (int l = 0; l < C; l++){
      yb += sol.y[l] * b[l];
    }
    */

    if (max_min){
      sol_eq = (xc + yb == 0);
    } else {
      sol_eq = (xc == yb);
    }

    //SAT
    int sat = 1;

    fixed_point_precision_16_16 prod0 = (sf.m1 * sf.x0) +
                                        (sf.m2 * sf.x1);

    fixed_point_precision_16_16 prod1 = (sf.m12 * sf.x0) +
                                        (sf.m13 * sf.x1);
    fixed_point_precision_16_16 prod2 = (sf.m23 * sf.x0) +
                                        (sf.m24 * sf.x1);
    fixed_point_precision_16_16 prod3 = (sf.m34 * sf.x0) +
                                        (sf.m35 * sf.x1);
    fixed_point_precision_16_16 prod4 = (sf.m45 * sf.x0) +
                                        (sf.m46 * sf.x1);
    fixed_point_precision_16_16 prod5 = (sf.m56 * sf.x0) +
                                        (sf.m57 * sf.x1);


    /*
    fixed_point_precision_16_16 prod[p_tableau.rows];
    for (int a=0;a<p_tableau.rows;a++){
      prod[a]=0;
    }
    for (int a=0;a<p_tableau.rows;a++){
      for (int b=0;b<V;b++){
        prod[a] += (get(a,(b+1),p_tableau)*sol.x[b]); //don't need 1st col
      }
    }
    */


    //eq?
    if (max_min){ // max prob
      sat = (d_equal(sf.m0,prod0) || sf.m0 > prod0) &&
            (d_equal(sf.m11,prod1) || sf.m11 > prod1) &&
            (d_equal(sf.m22,prod2) || sf.m22 > prod2) &&
            (d_equal(sf.m33,prod3) || sf.m33 > prod3) &&
            (d_equal(sf.m44,prod4) || sf.m44 > prod4) &&
            (d_equal(sf.m55,prod5) || sf.m55 > prod5);
        /*
       for (int c = 1; c < p_tableau.rows; c++){
          sat = sat && (d_equal(get(c,0,p_tableau),prod[c]) || get(c,0,p_tableau) > prod[c]);
        }
        */
    } else { // min prob
      sat = (d_equal(sf.m0,prod0) || sf.m0 < prod0) &&
            (d_equal(sf.m11,prod1) || sf.m11 < prod1) &&
            (d_equal(sf.m22,prod2) || sf.m22 < prod2) &&
            (d_equal(sf.m33,prod3) || sf.m33 < prod3) &&
            (d_equal(sf.m44,prod4) || sf.m44 < prod4) &&
            (d_equal(sf.m55,prod5) || sf.m55 < prod5);
      /*
       for (int d = 1; d < p_tableau.rows; d++){
          sat = sat && (d_equal(get(d,0,p_tableau),prod[d]) || get(d,0,p_tableau) < prod[d]);
        }
        */
    }


    return sol_eq && sat;
}


Solution_Box simplex_gadget(Tableau p_tableau, int p_max_min) {

  Solution_Box sol = __GADGET_compute(simplex_prover(p_tableau, p_max_min));
  //printf("CHECK %d", simplex_check(p_max_min, sol));
  __GADGET_check(simplex_check(p_max_min, sol));

  return sol;
}
