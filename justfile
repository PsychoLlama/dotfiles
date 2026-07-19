# Lint, typecheck, and test Lua sources.
mod lua 'lua.just'

# Manage custom package derivations.
mod packages 'pkgs/justfile'

set indentation := "  "
set default-list

# Activate the current NixOS configuration.
activate mode="test":
  #!/usr/bin/env -S bash -eu
  sudo true # Prompt for sudo up front.
  nh os {{ mode }} .

# Build the NixOS configuration without activating.
build:
  nh os build .

# Format all files.
fmt:
  treefmt

# Check that all files are formatted.
fmt-check:
  treefmt --ci

# Update all flake inputs and custom packages.
update:
  nix flake update
  @just packages update

# Run all checks, reporting all failures.
check:
  #!/usr/bin/env bash
  failed=0
  echo "--- Checking formatting ---"
  just fmt-check || failed=1
  echo "--- Running linter ---"
  just lua lint || failed=1
  echo "--- Running type checker ---"
  just lua typecheck || failed=1
  echo "--- Running unit tests ---"
  just lua test || failed=1
  echo "--- Building NixOS configuration ---"
  just build || failed=1
  exit $failed
