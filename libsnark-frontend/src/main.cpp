#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <vector>
#include <tuple>
#include <fstream>
#include <iostream>
#include <sstream>

#include "libff/algebra/fields/field_utils.hpp"
#include "libsnark/relations/constraint_satisfaction_problems/r1cs/r1cs.hpp"
#include "libsnark/relations/variable.hpp"
#include "libsnark/zk_proof_systems/ppzksnark/r1cs_ppzksnark/r1cs_ppzksnark.hpp"
#include "libsnark/common/default_types/r1cs_ppzksnark_pp.hpp"
#include "libsnark/gadgetlib1/pb_variable.hpp"

using namespace libsnark;
using namespace std;

void usage(const char * argv0)
{
  cerr << "Usage: " << argv0 << " " << "<path_to_r1cs_file>" << endl;

}

typedef libff::Fr<default_r1cs_ppzksnark_pp> FieldT;

using LC = vector<pair<uint64_t, pb_variable<FieldT>>>;
using Constraint = tuple<LC, LC, LC>;

linear_combination<FieldT> read_combination(ifstream& in, const vector<pb_variable<FieldT>>& variables)
{
  uint64_t n_terms;
  uint64_t coeff;
  uint64_t var;
  in >> n_terms;
  linear_combination<FieldT> a;
  for (uint64_t j = 0; j < n_terms; j++) {
    in >> coeff;
    in >> var;
    if (var == 1) {
      a.add_term(linear_term<FieldT>(0, coeff));
    } else {
      a.add_term(linear_term<FieldT>(variables[var-2], coeff));
    }
  }
  return a;
}

int main(int argc, char * argv[])
{
  if (argc != 2) {
    usage(argv[0]);
    exit(2);
  }

  default_r1cs_ppzksnark_pp::init_public_params();
  ifstream in{argv[1]};

  uint64_t n_inputs;
  uint64_t n_witnesses;
  uint64_t n_constraints;
  in >> n_inputs >> n_witnesses >> n_constraints;

  protoboard<FieldT> pb;

  vector<pb_variable<FieldT>> variables;

  for (uint64_t i = 0; i < n_inputs + n_witnesses; i++) {
    variables.push_back(pb_variable<FieldT>());
    ostringstream var_name;
    var_name << (i < n_inputs ? "x" : "w") << (i + 2);
    variables.back().allocate(pb, var_name.str());
  }
  pb.set_input_sizes(n_inputs);

  vector<Constraint> constraints;
  for (uint64_t i = 0; i < n_constraints; i++) {
    linear_combination<FieldT> a = read_combination(in, variables);
    linear_combination<FieldT> b = read_combination(in, variables);
    linear_combination<FieldT> c = read_combination(in, variables);
    pb.add_r1cs_constraint(r1cs_constraint<FieldT>(a, b, c));
  }

  //// Add witness values

  //pb.val(x) = 3;
  //pb.val(out) = 35;
  //pb.val(sym_1) = 9;
  //pb.val(y) = 27;
  //pb.val(sym_2) = 30;

  const r1cs_constraint_system<FieldT> constraint_system = pb.get_constraint_system();

  const r1cs_ppzksnark_keypair<default_r1cs_ppzksnark_pp> keypair = r1cs_ppzksnark_generator<default_r1cs_ppzksnark_pp>(constraint_system);

  //const r1cs_ppzksnark_proof<default_r1cs_ppzksnark_pp> proof = r1cs_ppzksnark_prover<default_r1cs_ppzksnark_pp>(keypair.pk, pb.primary_input(), pb.auxiliary_input());

  //bool verified = r1cs_ppzksnark_verifier_strong_IC<default_r1cs_ppzksnark_pp>(keypair.vk, pb.primary_input(), proof);

  cout << "Number of R1CS constraints: " << constraint_system.num_constraints() << endl;
  cout << "Primary (public) input: " << pb.primary_input() << endl;
  cout << "Auxiliary (private) input: " << pb.auxiliary_input() << endl;
  //cout << "Verification status: " << verified << endl;

  return 0;
}
