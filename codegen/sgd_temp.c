typedef double fp64;

// Iterate over d
int grad_check($ws, $xis, short int y) {
  return y*($dot_wsxis) >= 1;
}

int main() {

    long int $existentials;

    __GADGET_sgd($d, $n,
        $dataset
    );

    int check = __GADGET_check(
        $grad_checks);

    return check;
}

