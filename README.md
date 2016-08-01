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

This particular branch implements an early version of the trace-based synthesis
algorithm that symbolically (and abstractly) interprets the schedule sketch (with
holes encoded as symbolic choices), assigning each attribute a unique step number
corresponding to its position in a fine-grained trace of the schedule on the
tree. Each attribute's step number is then asserted to be greater than each of
its definition's dependencies' step numbers.

Note that a single definition may correspond to several step numbers. The
symbolic abstract interpretation of the schedule language ensures that
constraints on individual step numbers correctly influence the possible choices
for each hole in the schedule. Practically, these holes need to be used sparingly
and in places that do not affect control-flow; specifically, holes are only
feasible to solve if they replace perhaps a handful of visit slots in the entire
schedule, leaving traversal types and compositions concrete.

## Synthesis Engine

These directories contains code in [Typed] Racket and
(Rosette)[http://emina.github.io/rosette/] for the parsing, transformation,
serialization, and angelic evaluation (interpretation) of derivations of an
attribute grammar given as FTL (backronym: "Functional Tree Language") code. In
the future,  this will also include optimal schedule synthesis,
incrementalization with optimal synthesis of change propagation functions, and
code generation (compilation) targeting a variety of backends (language + tree +
library).

### Attribute Grammar (`grammar/`)

This directory contains code for parsing, serializing, manipulating,
typechecking,and translating to an intermediate representation the AST of the
attribute grammar domain-specific language (DSL), which is FTL.

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
