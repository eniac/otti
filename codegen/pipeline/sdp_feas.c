typedef float fp64;

#define epsilon (fp64)1.0e-2


int d_equal(fp64 a, fp64 b) {
  if ((a-b) > 0) {
    return (a-b) < epsilon;
  } else {
    return -1*(a-b);
  }
}


//n,m, C, X, big array of A's, b, sol_y, sol_x
int check_sdp(int n,int m,$fp){

  int solved = 1;

$det1


$a_x

$s_mat

$det2


fp64 gap = $gap;

solved = solved && (d_equal(gap,0.0));

return solved;

}


int sdp_instance(){

$xvars

$yvars

//n,m, C, X, big array of A's, b, sol_y, sol_x
__GADGET_sdp($seq1);

int check = __GADGET_check(check_sdp($seq2));

return check;

}
