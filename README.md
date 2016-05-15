# Synthesis of Incremental and Parallel Attribute Grammar Evaluators

This is a work in progress. If you're interested in the background for this
research project, take a look at
[Superconductor](https://github.com/Superconductor/superconductor), which is the
culmination of a substantial amount of prior work in this subject.

At a high level, the vision for this project is to optimally synthesize parallel,
incremental attribute grammar evaluators specialized to interesting subsets of
the domain (or just the whole domain). The concrete motivating problem is make
possible high-performance layout of extremely large data visualizations (cf.,
"big data").

## Synthesis Engine

These directories contains code in [Typed] Racket and
(Rosette)[http://emina.github.io/rosette/] for the parsing, transformation,
serialization, and angelic evaluation (interpretation) of derivations of an
attribute grammar given as FTL (backronym: "Functional Tree Language") code. In
the future,  this will also include optimal schedule synthesis,
incrementalization with optimal synthesis of change propagation functions, and
code generation (compilation) targeting a variety of backends (language + tree +
library).

### Core (`core/`)

This directory contains generic utility functions and code for creating and
manipulating the AST of the attribute grammar domain-specific language (DSL),
which is FTL.

### Compilation (`compile/`)

This directory contains code for parsing, typechecking, serializing, and
translating to an intermediate representation a program in the attribute grammar
DSL.

### Angelic Evaluator (`angelic/`)

This directory contains code to evaluate a tree of an attribute grammar
angelically. It can be thought of as a solver-aided specification for the
correctness of attribute grammar evaluation.

### Scheduled Evaluator (`schedule/`)

This directory contains the code for parsing, serializing, interpreting, and
synthesizing (from a sketch) a schedule. This will be redesigned to support
"partial evaluation" with respect to a schedule, in order to generate the
corresponding layout engine identified by the schedule.

## Browser (`browser/`)

This directory contains the code for our flagship application of the synthesis
engine, which is an (approximately) optimally parallel and incremental web
browser that synthesizes specialized layout engines as needed. An initial version
will support a somewhat limited subset of HTML and CSS without JavaScript and
render to an image. This "browser" is intended only as a proof-of-concept, and
once that purpose is fulfilled, we will move onto integrating our synthesized
layout engine with existing, commercial web browsers.

This will be written after the completion of the synthesis engine and is thus
almost entirely unimplemented.
