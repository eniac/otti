#!/usr/bin/env zsh
# Runs end-to-end workflow tests.
set -ex

function num_constraints() {
  cat C | head -n 1 | awk '{ print $3 }'
}
echo "a 10\nb 10" > i
stack run --profile -- c max ./test/Code/C/max.c --setup -i i --streams values,gadgets::user::analytics
stack run --profile -- c max ./test/Code/C/max.c --prove -i i
stack run -- verify

