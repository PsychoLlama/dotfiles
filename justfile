_:
  just --list

# Activate the current NixOS configuration.
activate mode="test":
  sudo true # Prompt for password
  sudo nixos-rebuild --log-format internal-json -v {{mode}} --flake . 2>&1 | nom --json

# Build the NixOS configuration without activating.
build:
  nixos-rebuild --log-format internal-json -v build --flake . 2>&1 | nom --json

# Build the documentation website.
build-docs:
  nix build '.#docs-website'

# Format all files.
fmt:
  treefmt

# Run luacheck on all Lua files.
lint:
  luacheck pkgs platforms

# Run Lua unit tests with vusted.
unit-test:
  vusted pkgs platforms

# Run Lua unit tests against the built editor.
unit-test-built:
  #!/usr/bin/env bash
  set -euo pipefail
  editor=$(nix build '.#editor' --no-link --print-out-paths)
  packdir=$(grep -oP "packpath\^=\K[^\"]+" "$editor/bin/nvim")
  lab_nvim=$(readlink -f "$packdir/pack/managed-by-nix/opt/lab.nvim")
  PATH="$editor/bin:$PATH" vusted --lpath="$lab_nvim/lua/?.lua;$lab_nvim/lua/?/init.lua" pkgs platforms

# Check that all files are formatted.
fmt-check:
  treefmt --fail-on-change

# Run lua-language-server type checks.
typecheck:
  #!/usr/bin/env bash
  set -euo pipefail
  export VIMRUNTIME=$(nvim --clean --headless --cmd 'echo $VIMRUNTIME | q' 2>&1)
  lua-language-server --check . --checklevel=Warning

# Update all flake inputs and custom packages.
update:
  nix flake update
  @just update-packages

# Update all custom packages.
update-packages:
  nix-update --flake chrome-devtools-mcp --version-regex 'chrome-devtools-mcp-v(.*)'

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
  just unit-test-built || failed=1
  echo "--- Building NixOS configuration ---"
  just build || failed=1
  echo "--- Building documentation ---"
  just build-docs || failed=1
  exit $failed
