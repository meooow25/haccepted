#!/bin/bash

set -e

csvfile="bench-out/bench-out.csv"
summaryfile="bench-out/bench-out.txt"
rm -f "${csvfile}"
stack build
stack bench --benchmark-arguments="--csv ${csvfile}"
stack exec gen-md "${csvfile}" "${summaryfile}"
