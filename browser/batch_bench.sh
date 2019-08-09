#!/bin/bash

DIRECTORY="examples/bug"

CASSIUS="../../Cassius/src/run.rkt"
PROBLEM="doc-2"

function cassius {
  racket ${CASSIUS} accept $1 ${PROBLEM}
}

trap 'trap - INT; kill -s INT "$$"' INT

for benchmark in  $(cd ${DIRECTORY}; ls *.html | sed -e 's/\([a-z0-9-]*\)\.html/\1/g')
do
  printf "\n\e[1m[BENCHMARK ${benchmark}]\e[0m\n"
  report="${DIRECTORY}/${benchmark}.out.cassius"
  rm ${report} &>/dev/null
  cargo run --quiet --release -- --bench ${benchmark} --cassius &&
  cassius ${report}
done
