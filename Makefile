# Run Willy.
run:
	cargo run -- main.scm

# Run Willy in release mode.
run-release:
	cargo run --release -- main.scm

# Run Willy starting in the Guile Scheme shell.
run-shell:
	cargo run

# Build Willy.
build:
	cargo build

# Run all Willy tests.
test:
	make test-rust
	make test-rust-doc
	make test-scheme

# Run Rust tests.
test-rust:
	cargo nextest run

# Run Rust doc tests.
test-rust-doc:
	cargo test --doc

# Run Scheme tests.
test-scheme:
	find scheme/tests -type f -name "*.scm" -exec cargo run -- {} \;
