_:
  just --list

# Activate the current NixOS configuration.
activate mode="test":
  #!/usr/bin/env bash
  sudo true # Prompt for sudo up front.
  nh os {{mode}} .

# Build the NixOS configuration without activating.
build:
  nh os build .

# Format all files.
fmt:
  treefmt

# Run luacheck on all Lua files.
lua-lint:
  luacheck pkgs platforms

# Run Lua unit tests with vusted.
lua-test:
  vusted pkgs platforms

# Run Lua unit tests against the built editor.
lua-test-built:
  #!/usr/bin/env bash
  set -euo pipefail
  editor=$(nix build '.#editor' --no-link --print-out-paths)
  packdir=$(grep -oP "packpath\^=\K[^\"]+" "$editor/bin/nvim")
  lab_nvim=$(readlink -f "$packdir/pack/managed-by-nix/opt/lab.nvim")
  note_nvim=$(readlink -f "$packdir/pack/managed-by-nix/opt/note.nvim")
  PATH="$editor/bin:$PATH" vusted --lpath="$lab_nvim/lua/?.lua;$lab_nvim/lua/?/init.lua;$note_nvim/lua/?.lua;$note_nvim/lua/?/init.lua" pkgs platforms

# Check that all files are formatted.
fmt-check:
  treefmt --ci

# Run lua-language-server type checks.
lua-typecheck:
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
  ./pkgs/codex-bin/update.sh
  ./pkgs/claude-code-bin/update.sh

# Run all checks, reporting all failures.
check:
  #!/usr/bin/env bash
  failed=0
  echo "--- Checking formatting ---"
  just fmt-check || failed=1
  echo "--- Running linter ---"
  just lua-lint || failed=1
  echo "--- Running type checker ---"
  just lua-typecheck || failed=1
  echo "--- Running unit tests ---"
  just lua-test-built || failed=1
  echo "--- Building NixOS configuration ---"
  just build || failed=1
  exit $failed
