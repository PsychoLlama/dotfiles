set shell := ["nu", "-c"]

_:
  just --list

# Activate the current NixOS configuration.
activate mode="test":
  sudo true # Prompt for password
  sudo nixos-rebuild --log-format internal-json -v {{mode}} --flake . err+out>| nom --json

# Format all files.
fmt:
  treefmt

# Run luacheck on all Lua files.
lint:
  luacheck pkgs platforms

# Run Lua unit tests with vusted.
unit:
  vusted pkgs platforms

# Run lua-language-server type checks.
typecheck:
  #!/usr/bin/env bash
  set -euo pipefail
  export VIMRUNTIME=$(nvim --clean --headless --cmd 'echo $VIMRUNTIME | q' 2>&1)
  lua-language-server --check . --checklevel=Warning

# Run all checks (lint + typecheck + unit tests), reporting all failures.
test:
  #!/usr/bin/env bash
  failed=0
  just lint || failed=1
  just typecheck || failed=1
  just unit || failed=1
  exit $failed
