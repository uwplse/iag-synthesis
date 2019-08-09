#!/bin/bash

DIRECTORY="examples/sanity"

CASSIUS="../../Cassius/src/run.rkt"
PROBLEM="doc-2"

function cassius {
  racket ${CASSIUS} accept $1 ${PROBLEM}
}

trap 'trap - INT; kill -s INT "$$"' INT

for testcase in  $(cd ${DIRECTORY}; ls *.html | sed -e 's/\([a-z0-9-]*\)\.html/\1/g')
do
  printf "\n\e[1m[TESTCASE ${testcase}]\e[0m\n"
  report="${DIRECTORY}/${testcase}.out.cassius"
  rm ${report} &>/dev/null
  cargo run --quiet --release -- --test ${testcase} --cassius &&
  cassius ${report}
done
