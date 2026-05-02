# okasaki-study

Developing ocaml:

- dune and an ocaml compiler
- Tests are simple functors over data structure variants with `assert`
> dune test --no-buffer

Goals:

- A tour of Purely Functional Data Structures by Chris Okasaki in idiomatic modern OCaml
- Do the same tour in Rust once I validate I understand the algorithms
- Use the Rust implementation to create a persistent search tree with subtree refs cached in a map to analyze chess positions driven by LLM exploration. It wont beat Stockfish or AlphaZero but it will demo practical immutable data in an agentic system.

Expectations:
- Write all of the implementation code myself
- Exceptions and deviations are noted in chapter-level notes

AI usage policy:
- Review syntax or compilation errors (I'm not an OCaml or Rust expert at the time of starting this)
- Recommend test cases but implement them myself
- Discuss or pressure-test observations, but only use my own words
- The LLM harness will obviously use a frontier provider API
