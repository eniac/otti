#!/bin/bash
flatc -o outs schema/zkinterface.fbs --json --strict-json -- $1
filename="$(basename $1 .zkif)"
json="$filename".json
less outs/$json

