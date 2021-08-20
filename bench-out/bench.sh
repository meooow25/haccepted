#!/bin/bash

set -e

tmpCsvFile="bench-out/bench-out.tmp.csv"
csvFile="bench-out/bench-out.csv"
summaryFile="bench-out/bench-out.txt"

rm -f "${tmpCsvFile}"
stack build
stack bench --benchmark-arguments="--csv ${tmpCsvFile}"
stack exec gen-summary "${tmpCsvFile}" "${csvFile}" "${summaryFile}"
rm "${tmpCsvFile}"
