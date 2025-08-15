#!/bin/bash

# Colors for output
RED=$(printf '\033[0;31m')
GREEN=$(printf '\033[0;32m')
YELLOW=$(printf '\033[1;33m')
NC=$(printf '\033[0m') # No Color

echo "Running CoC Type Checker Integration Tests"
echo "==========================================="

# Run the integration test
cargo test --test integration_test test_all_examples -- --nocapture 2>/dev/null | sed -E "s/^(.*✓ PASS.*)$/${GREEN}\1${NC}/; s/^(.*✗ FAIL.*)$/${RED}\1${NC}/"

# Also run just the implicit argument tests
echo ""
echo "Running Implicit Argument Tests Only"
echo "====================================="
cargo test --test integration_test test_implicit_examples -- --nocapture 2>/dev/null | sed -E "s/^(.*✓ PASS.*)$/${GREEN}\1${NC}/; s/^(.*✗ FAIL.*)$/${RED}\1${NC}/" | grep -A 20 "Testing Implicit"
