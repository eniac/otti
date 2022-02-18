#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <string>
#include <vector>
#include <tuple>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <getopt.h>

#include "libff/algebra/fields/field_utils.hpp"
#include "libsnark/gadgetlib1/protoboard.hpp"
#include "libsnark/relations/constraint_satisfaction_problems/r1cs/r1cs.hpp"
#include "libsnark/relations/variable.hpp"
#include "libsnark/zk_proof_systems/ppzksnark/r1cs_ppzksnark/r1cs_ppzksnark.hpp"
#include "libsnark/common/default_types/r1cs_ppzksnark_pp.hpp"
#include "libsnark/gadgetlib1/pb_variable.hpp"
#include "libsnark/zk_proof_systems/ppzksnark/r1cs_ppzksnark/r1cs_ppzksnark_params.hpp"

using namespace libsnark;
using namespace std;

void usage(const char * argv0)
{
  cerr << "Usage: " << argv0 << " [options] <command>" << endl
       << endl
       << "Options:" << endl
       << "  -P, --prover-parameters <file>    Where to place/fetch prover parameters" << endl
       << "  -V, --verifier-parameters <file>  Where to place/fetch verifier parameters" << endl
       << "  -p, --proof <file>                Where to place/fetch the proof" << endl
       << "  -C, --circuit <file>              Where to fetch the proof" << endl
       << "  -x, --public-inputs <file>        Where to fetch the public inputs" << endl
       << "  -w, --witness <file>              Where to fetch the witness" << endl
       << endl
       << "Commands:" << endl
       << "  setup   (requires --prover-parameters, --verifier-parameters, --circuit)" << endl
       << "  prove   (requires --prover-parameters, --public-inputs, --witness, --proof)" << endl
       << "  verify  (requires --verifier-parameters, --public-inputs, --proof)" << endl
       << "  all     (requires all: produces parameters and proof)" << endl
 ;
}

void usage_assert(const char * argv0, bool condition)
{
  if (not condition)
  {
    usage(argv0);
    exit(2);
  }
}

void my_assert(const char * message, bool condition)
{
  if (not condition)
  {
    cout << message << endl;
    exit(2);
  }
}

typedef libff::Fr<default_r1cs_ppzksnark_pp> FieldT;

using LC = vector<pair<uint64_t, pb_variable<FieldT>>>;
using Constraint = tuple<LC, LC, LC>;

linear_combination<FieldT> read_combination(ifstream& in, const vector<pb_variable<FieldT>>& variables)
{
  uint64_t n_terms;
  FieldT coeff;
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

protoboard<FieldT> read_protoboard(ifstream& in)
{
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
  uint64_t test;
  in >> test;
  my_assert("Input remains after protoboard", in.eof());
  return pb;
}

vector<FieldT> read_inputs(ifstream& in)
{
  uint64_t n_inputs;
  in >> n_inputs;
  vector<FieldT> xs;
  for (uint64_t i = 0; i < n_inputs; ++i)
  {
    xs.push_back(0);
    in >> xs.back();
  }
  uint64_t test;
  in >> test;
  my_assert("Input remains after inputs: ", in.eof());
  return xs;
}

int main(int argc, char * argv[])
{
  string command;
  // Paths
  string p_prover_parameters;
  string p_verifier_parameters;
  string p_proof;
  string p_circuit;
  string p_public_inputs;
  string p_witness;

  struct option long_options[] = {
    {"prover-parameters",   required_argument, nullptr, 'P'},
    {"verifier-parameters", required_argument, nullptr, 'V'},
    {"proof",               required_argument, nullptr, 'p'},
    {"circuit",             required_argument, nullptr, 'C'},
    {"public-inputs",       required_argument, nullptr, 'x'},
    {"witness",             required_argument, nullptr, 'w'},
    {nullptr,               0,                 nullptr, 0},
  };

  while (true) {
    const int opt  = getopt_long(argc, argv, "P:V:p:C:x:w:", long_options, NULL);

    if (opt < 0) {
      break;
    }

    switch (opt) {
    case 'P':
      p_prover_parameters = optarg;
      break;

    case 'V':
      p_verifier_parameters = optarg;
      break;

    case 'p':
      p_proof = optarg;
      break;

    case 'C':
      p_circuit = optarg;
      break;

    case 'x':
      p_public_inputs = optarg;
      break;

    case 'w':
      p_witness = optarg;
      break;
    }
  }

  if (optind != argc - 1) {
    usage(argv[0]);
    exit(2);
  } else {
    command = argv[optind];
  }

  default_r1cs_ppzksnark_pp::init_public_params();

  if (command == "setup") {
    usage_assert(argv[0], not p_prover_parameters.empty());
    usage_assert(argv[0], not p_verifier_parameters.empty());
    usage_assert(argv[0], not p_circuit.empty());

    ifstream in{p_circuit};
    my_assert("Could not open circuit file", in.good());
    protoboard<FieldT> pb = read_protoboard(in);
    const r1cs_constraint_system<FieldT> constraint_system = pb.get_constraint_system();
    cout << "Number of R1CS constraints: " << constraint_system.num_constraints() << endl;
    const r1cs_ppzksnark_keypair<default_r1cs_ppzksnark_pp> keypair = r1cs_ppzksnark_generator<default_r1cs_ppzksnark_pp>(constraint_system);
    ofstream f_prover_parameters{p_prover_parameters};
    f_prover_parameters << keypair.pk;
    ofstream f_verifier_parameters{p_verifier_parameters};
    f_verifier_parameters << keypair.vk;
  } else if (command == "verify") {
    usage_assert(argv[0], not p_verifier_parameters.empty());
    usage_assert(argv[0], not p_public_inputs.empty());
    usage_assert(argv[0], not p_proof.empty());

    ifstream f_verifier_parameters{p_verifier_parameters};
    my_assert("Could not open verifier parameters file", f_verifier_parameters.good());
    ifstream f_public_inputs{p_public_inputs};
    my_assert("Could not open public input file", f_public_inputs.good());
    ifstream f_proof{p_proof};
    my_assert("Could not open proof file", f_proof.good());

    r1cs_ppzksnark_verification_key<default_r1cs_ppzksnark_pp> vk;
    f_verifier_parameters >> vk;
    vector<FieldT> x = read_inputs(f_public_inputs);
    r1cs_ppzksnark_proof<default_r1cs_ppzksnark_pp> pf;
    f_proof >> pf;

    bool verified = r1cs_ppzksnark_verifier_strong_IC<default_r1cs_ppzksnark_pp>(vk, x, pf);
    my_assert("Verification failed", verified);
  } else if (command == "prove") {
    usage_assert(argv[0], not p_prover_parameters.empty());
    usage_assert(argv[0], not p_public_inputs.empty());
    usage_assert(argv[0], not p_witness.empty());
    usage_assert(argv[0], not p_proof.empty());
    ifstream f_prover_parameters{p_prover_parameters};
    my_assert("Could not open prover parameters file", f_prover_parameters.good());
    ifstream f_verifier_parameters{p_verifier_parameters};
    my_assert("Could not open verifier parameters file", f_verifier_parameters.good());
    ifstream f_public_inputs{p_public_inputs};
    my_assert("Could not open public input file", f_public_inputs.good());
    ifstream f_witness{p_witness};
    my_assert("Could not open witness file", f_witness.good());

    r1cs_ppzksnark_proving_key<default_r1cs_ppzksnark_pp> pk;
    f_prover_parameters >> pk;
    r1cs_ppzksnark_verification_key<default_r1cs_ppzksnark_pp> vk;
    f_verifier_parameters >> vk;
    vector<FieldT> x = read_inputs(f_public_inputs);
    vector<FieldT> w = read_inputs(f_witness);

    const r1cs_ppzksnark_proof<default_r1cs_ppzksnark_pp> proof = r1cs_ppzksnark_prover<default_r1cs_ppzksnark_pp>(pk, x, w);
    ofstream f_proof{p_proof};
    f_proof << proof;
    bool verified = r1cs_ppzksnark_verifier_strong_IC<default_r1cs_ppzksnark_pp>(vk, x, proof);
    my_assert("Verification failed", verified);
  } else if (command == "all") {
    usage_assert(argv[0], not p_prover_parameters.empty());
    usage_assert(argv[0], not p_verifier_parameters.empty());
    usage_assert(argv[0], not p_public_inputs.empty());
    usage_assert(argv[0], not p_witness.empty());
    usage_assert(argv[0], not p_proof.empty());
    usage_assert(argv[0], not p_circuit.empty());
    ifstream in{p_circuit};
    my_assert("Could not open circuit file", in.good());
    ifstream f_public_inputs{p_public_inputs};
    my_assert("Could not open public input file", f_public_inputs.good());
    ifstream f_witness{p_witness};
    my_assert("Could not open witness file", f_witness.good());

    protoboard<FieldT> pb = read_protoboard(in);
    const r1cs_constraint_system<FieldT> constraint_system = pb.get_constraint_system();
    const r1cs_ppzksnark_keypair<default_r1cs_ppzksnark_pp> keypair = r1cs_ppzksnark_generator<default_r1cs_ppzksnark_pp>(constraint_system);

    vector<FieldT> x = read_inputs(f_public_inputs);
    vector<FieldT> w = read_inputs(f_witness);

    r1cs_ppzksnark_proof<default_r1cs_ppzksnark_pp> proof = r1cs_ppzksnark_prover<default_r1cs_ppzksnark_pp>(keypair.pk, x, w);
    ofstream f_proof{p_proof};
    f_proof << proof;
    ofstream f_vk{p_verifier_parameters};
    f_vk << keypair.vk;
    ofstream f_pk{p_prover_parameters};
    f_pk << keypair.pk;

    bool verified = r1cs_ppzksnark_verifier_strong_IC<default_r1cs_ppzksnark_pp>(keypair.vk, x, proof);
    cout << "Verification status: " << verified << endl;
  } else {
    usage(argv[0]);
    exit(2);
  }

  return 0;
}
