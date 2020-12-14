#!/usr/bin/env zsh
# Runs end-to-end workflow tests.
set -ex

function num_constraints() {
  cat C | head -n 1 | awk '{ print $3 }'
}
echo "a 20\nb 30" > i
stack run --profile -- c max ./test/Code/C/max.c --setup -i i --streams gadgets::user::analytics,gadgets::user::verification

