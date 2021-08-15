#!/bin/bash

set -e

csvfile="bench-out/bench-out.csv"
mdfile="bench-out/bench-out.md"
rm -f "${csvfile}"
stack build
stack bench --benchmark-arguments="--csv ${csvfile}"
stack exec gen-md "${csvfile}" "${mdfile}"
