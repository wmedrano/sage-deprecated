# Sage

Update: Retrying using a Scheme built for Rust: https://github.com/mattwparas/steel.

WIP: Sage is a work in progress, with the primary purpose being
educational. Do not expect this to go far.

A (Guile) Scheme configured IDE. Heavily inspired by Emacs.

## Goals

- Integrate Guile with Rust. The main framework for this is the custom
  `flashkick` crate of this repo.
- Highly responsiveness editing experience. Emacs can get jerky at
  times, especially with non-async auto-complete.

## Building, Running, and Testing

See `.github/workflows/testing.yml` building and testing recipes.
