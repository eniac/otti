# testcase-generation

Scripts for creating test proof instances. These scripts should serve as an example of how to provide an interface for the testing infrastructure to generate test instances in a semi-automated fashion.

## Running a single test family generator script with parameters
Each generation script for a single test family in this directory has variables of the following form:

- <script_name> <base_directory_name> <test_modulus> <size> optional: <max_range>
  -  <base_directory_name> is the target directory for the tests
  -  <test_modulus> is one of  p0, p1, p2, p3, p4; there are standard primes in `utils/sieve_test_constants.py`
  -  <size> is the smallest size variable, which differs for each script
  -  <max_range> if provided, creates tests for size through max_range, inclusive
  
## Using the config.json file

An entire set of proof statements is be specified in `config.json` and created with `generate_statements`.
- Execute as `./generate_statements <config_file>` (or `python3 generate_statements <config_file>`)
  - Creates test statements as specified by the configuration file
  - If no argument is given, it defaults to looking for `config.json`

The config file specifies:
- `target`: The directory to place generated test families in
- `test-families`: A dictionary. Each key is the name of a test family; the object for the key is another dictionary, containing the following information for the test family:
  - `primes`: Create the test set for each of the primes listed
    - This is an array with the possible values of ["p0", "p1", "p2", "p3", "p4"]
  - `min_size`: Integer with the minimum size of the scaling factor for the test family, e.g., 8
  - `max_size`: Integer with the maximum size (inclusive) of the scaling factor for the test family, e.g., 16
  - `size_notes`, a comment field explaining what `min_size` and `max_size` are selecting across (though this field is not used by the generation script)

Note that each test family has a different definition for what `size` means; this is meant to be a "reasonable" scaling factor.
We tried to make these scaling factors roughly exponential, with the scaling dominated by a power of two doubling at each step.

In our example, `generate_statements` is a Python script that calls each family generator script through the command line with parameters.
This is not a requirement; it is reasonable for a `generate_statements` to be any executable, so long as it can be configured through a configuration JSON file.

Running `generate_statements` with the example `config.json` file modifies the `../tests` directory.
It adds a directory for each pair of (test family, prime) specified, e.g. as `../tests/discrete-log_p1/`.
Each pair's directory contains a series of directories corresponding to values from `min_size` to `max_size` for that test family.
Each such directory contains `flatbuffer` and `text` directories, holding the proof statement as specified in the [run_tests README](/run_tests/README.md).

# TA1 Expectations

To allow the T&E team to generate test statement instances from TA1 in a streamlined fashion, we ask that each TA1 team provide a mechanism for generation in a similar manner.

## Creating your own scripts for TA1 integration
In particular, we need: 

- An example configuration JSON file, using:
  - A `target` field to place generated statements into
  - A dictionary of test families to generate statements in (one is fine). Each test family dictionary should contain:
  - `min_size` and `max_size`, the values for some reasonable scaling factor to span when generating statements
  - `primes`, specifying the primes to generate proofs for
  - Please also include an explanation of the meaning of `min_size` and `max_size` - it doesn't have to be in a `size_notes` field or directly in the configuration file, but we need to understand what the scaling factor you chose means and why you chose it.

- A `generate_statements` that can be executed as `./generate_statements <config file>`
  - This should generate a structure as above in the `target` directory: (test family, prime) -> size -> proof statement

Please also provide a Packer script [like the version used for proof execution](/packer) that installs dependencies and places your executable at `/home/ec2-user/generate_statements`.
