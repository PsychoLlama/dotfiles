_:
  just --list

# Activate the current NixOS configuration.
activate mode="test":
  sudo true # Prompt for password
  sudo nixos-rebuild --log-format internal-json -v {{mode}} --flake . 2>&1 | nom --json

# Build the NixOS configuration without activating.
build:
  nixos-rebuild --log-format internal-json -v build --flake . 2>&1 | nom --json

# Format all files.
fmt:
  treefmt

# Run luacheck on all Lua files.
lint:
  luacheck pkgs platforms

# Run Lua unit tests with vusted.
unit-test:
  vusted pkgs platforms

# Check that all files are formatted.
fmt-check:
  treefmt --fail-on-change

# Run lua-language-server type checks.
typecheck:
  #!/usr/bin/env bash
  set -euo pipefail
  export VIMRUNTIME=$(nvim --clean --headless --cmd 'echo $VIMRUNTIME | q' 2>&1)
  lua-language-server --check . --checklevel=Warning

# Run all checks, reporting all failures.
check:
  #!/usr/bin/env bash
  failed=0
  echo "--- Checking formatting ---"
  just fmt-check || failed=1
  echo "--- Running linter ---"
  just lint || failed=1
  echo "--- Running type checker ---"
  just typecheck || failed=1
  echo "--- Running unit tests ---"
  just unit-test || failed=1
  echo "--- Building NixOS configuration ---"
  just build || failed=1
  exit $failed
