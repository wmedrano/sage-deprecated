# Run Willy.
run:
	guile main.scm

# Build Willy.
build:
	make build-rust
	make build-scheme

build-rust:
	cargo build

build-scheme: target/debug/libwilly.so
	find scheme/ -type f -name "*.scm" -exec guild compile {} \;

# Run all Willy tests.
test:
	make test-rust
	make test-rustdoc
	make test-scheme

# Run Rust tests.
test-rust:
	cargo nextest run

# Run Rust doc tests.
test-rustdoc:
	cargo test --doc

# Run Scheme tests.
test-scheme:
	find scheme/tests -type f -name "*.scm" -exec guile {} \;
