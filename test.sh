#!/bin/sh

./run.rkt --root Tree layout benchmarks/css/toy.grammar
cd browser
cargo build && cargo run -- --html examples/margin.html --css examples/margin.css
