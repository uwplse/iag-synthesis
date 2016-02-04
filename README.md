# Incremental and Parallel Evaluation of Attribute Grammars Through Synthesis

This is a work in progress.

## Synthesis Engine (`./synthesis`)

This directory contains Rosette code for the parsing, transformation,
serialization, and angelic evaluation (interpretation) of derivations of an
attribute grammar given as FTL (backronym: "Functional Tree Language") code. In
the future,  this will also include optimal schedule synthesis,
incrementalization with optimal synthesis of change propagation (delta)
functions, and code generation (compilation) targeting a variety of backends
(language + tree + library).

## Layout Engine (`./layout`)

This directory contains the code for our flagship application of the synthesis
engine, which is an (approximately) optimally parallel and incremental layout
engine. An initial version will run in Rosette and support a limited subset of
HTML and CSS without JavaScript and render to an image.
