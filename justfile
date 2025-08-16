default:
    @just --list

install-docs:
    cargo install mdbook
    cargo install --git https://github.com/sdiehl/mdbook-include-rs.git

build-docs:
    cd docs && mdbook build

serve-docs:
    cd docs && mdbook serve --open

clean-docs:
    cd docs && mdbook clean

docs: build-docs serve-docs

build:
    cargo build --all

test:
    cargo test --all

format:
    cargo +nightly fmt --all

lint:
    cargo clippy --all -- -D warnings

dev:
    just format
    just lint
    just build
    just test
    just build-docs

clean-all:
    cargo clean
    just clean-docs
