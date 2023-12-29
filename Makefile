# Run Willy.
run:
	make build
	LD_LIBRARY_PATH=target/debug guile main.scm

run-release:
	cargo build --release
	LD_LIBRARY_PATH=target/release guile main.scm

# Build Willy.
build:
	make build-rust
	make build-scheme

build-rust:
	cargo build

build-scheme:
	LD_LIBRARY_PATH=target/debug find scheme/ -type f -name "*.scm" -exec guild compile {} \;

# Run all Willy tests.
test:
	make test-rust
	make test-rustdoc
	make test-scheme

# Run Rust tests.
test-rust:
	cargo test

# Run Rust doc tests.
test-rustdoc:
	cargo test --doc

# Run Scheme tests.
test-scheme:
	LD_LIBRARY_PATH=target/debug find scheme/tests -type f -name "*.scm" -exec guile {} \;
