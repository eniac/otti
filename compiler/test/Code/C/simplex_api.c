#include <stdio.h>
#include "simplex.h"


int main(void) {

  Tableau t0 = make_problem();

  Constraint obj = {{24.0, 60.0}, 0, 0};
  Tableau t1 = minimize(t0, obj);

  Constraint c1 = {{0.5, 1.0}, GEQ, 6.0};
  Constraint c2 = {{2.0, 2.0}, GEQ, 14.0};
  Constraint c3 = {{1.0, 4.0}, GEQ, 13.0};
  Tableau t2 = add(t1, c1);
  Tableau t3 = add(t2, c2);
  Tableau t4 = add(t3, c3);


  Solution solution_vec = simplex_gadget(t4, MIN);
  print_sol(solution_vec);

  return 0;

}
