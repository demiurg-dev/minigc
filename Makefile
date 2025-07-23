all: clippy fmt_check test

clippy:
	cargo clippy --no-deps --all-features --all-targets -- -D warnings

test:
	cargo test --all-features --all-targets

fmt_check:
	cargo +nightly fmt -- --check

fmt:
	cargo +nightly fmt

build:
	cargo build

release:
	cargo build --release
