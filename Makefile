# Build
build:
	make build-rust
	make build-scheme

build-rust:
	cargo build

build-scheme:
	LD_LIBRARY_PATH=target/debug guild compile config.scm
	LD_LIBRARY_PATH=target/debug find scheme -type f -name "*.scm" -exec guild compile {} \;

# Run
run:
	LD_LIBRARY_PATH=target/debug guile config.scm --debug

run-release:
	cargo build --release
	LD_LIBRARY_PATH=target/release guile config.scm

flamegraph-profile:
	cargo build --release
	LD_LIBRARY_PATH=target/release flamegraph -- guile config.scm

# Test
test:
	make test-rust
	make test-rustdoc
	make test-scheme

test-rust:
	cargo nextest run

test-rustdoc:
	cargo test --doc

test-scheme:
	LD_LIBRARY_PATH=target/debug find scheme/tests -type f -name "*.scm" -exec guile {} \;
