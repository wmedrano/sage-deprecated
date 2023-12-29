# Run Willy.
run:
<<<<<<< HEAD
	make build
	LD_LIBRARY_PATH=target/debug guile main.scm

run-release:
	cargo build --release
	LD_LIBRARY_PATH=target/release guile main.scm
=======
	cargo run -- main.scm

# Run Willy in release mode.
run-release:
	cargo run --release -- main.scm

# Run Willy starting in the Guile Scheme shell.
run-shell:
	cargo run
>>>>>>> main

# Build Willy.
build:
	make build-rust
	make build-scheme

build-rust:
	cargo build

build-scheme:
<<<<<<< HEAD
	LD_LIBRARY_PATH=target/debug find scheme/ -type f -name "*.scm" -exec guild compile {} \;
=======
	find scheme/ -type f -name "*.scm" -exec guild compile {} \;
>>>>>>> main

# Run all Willy tests.
test:
	make test-rust
<<<<<<< HEAD
	make test-rustdoc
=======
	make test-rust-doc
>>>>>>> main
	make test-scheme

# Run Rust tests.
test-rust:
	cargo nextest run

# Run Rust doc tests.
test-rustdoc:
	cargo test --doc

# Run Scheme tests.
test-scheme:
<<<<<<< HEAD
	LD_LIBRARY_PATH=target/debug find scheme/tests -type f -name "*.scm" -exec guile {} \;
=======
	find scheme/tests -type f -name "*.scm" -exec cargo run -- {} \;
>>>>>>> main
