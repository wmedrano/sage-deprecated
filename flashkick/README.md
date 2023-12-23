# Flashkick

A Rust wrapper over GNU Guile Scheme.

## Unsafe

Currently the API is unsafe as it may not be the proper way to integrate with
GNU Guile. Known issues:

- Not thread safe. Running in multiple threads may lead to SIGABRT.
- Interactions with the GC have not been verified.
