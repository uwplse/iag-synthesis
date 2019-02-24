# Schedule Synthesis Evaluation

Make sure you have a recent version of Rosette installed as well as the data package for some additional data structures:

```bash
> raco pkg install rosette
> raco pkg install data
```

To try out some synthesis, open up `run.rkt` in either Emacs with Racket mode or DrRacket. There are some tests preconfigured for two attribute grammars: HVBox and Treemap. The provided sets of example inputs are in the variables `{hvbox,treemap}-forest`, and the provided sketches are in the variables `{hvbox,treemap}-skeleton`. (All that can be found automatically but is essentially cached for quick testing.) Run the synthesizer backed by the symbolic trace with `(tracing:test-{hvbox,treemap})` and the synthesizer based on general-purpose symbolic evaluation with `(checking:test-{hvbox,treemap})`. All these tests leave their constraints in the assertion store, so you can inspect them with `(asserts)`. The synthesizer based on the symbolic trace can be run again in the same session after emptying the assertion store with `(clear-asserts!)`, but the other synthesizer mutates the state of the example trees. In other words, be sure to restart your REPL session in between runs of the synthesizer based on general-purpose symbolic evaluation.
