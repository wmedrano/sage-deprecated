# Willy

WIP: Willy is a work in progress, with the primary purpose being
educational. Do not expect this to go far.

A (Guile) Scheme configured IDE. Heavily inspired by Emacs.

## Goals

- Integrate Guile with Rust. The main framework for this is the custom
  `flashkick` crate of this repo.
- Highly responsiveness editing experience. Emacs can get jerky at
  times, especially with non-async auto-complete.

## Building, Running, and Testing

See `.github/workflows/testing.yml` for building and testing recipes.

To run the program, run: `cargo run --release -- -l scheme/ main.scm`. This:

- Starts the Willy Guile program. This is basically Guile with Willy
  functions linkend in.
- Adds the `scheme` directory to the load path. This provides access
  to the Willy Scheme modules.
- Runs the main.scm program.
