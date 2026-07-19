_:
  just --list

# Activate the current NixOS configuration.
activate mode="test":
  #!/usr/bin/env -S bash -eu
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
  dotfiles_nvim=$(readlink -f "$packdir/pack/managed-by-nix/opt/dotfiles.nvim")
  note_nvim=$(readlink -f "$packdir/pack/managed-by-nix/opt/note.nvim")
  PATH="$editor/bin:$PATH" vusted --lpath="$dotfiles_nvim/lua/?.lua;$dotfiles_nvim/lua/?/init.lua;$note_nvim/lua/?.lua;$note_nvim/lua/?/init.lua" pkgs platforms

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

# Update the chrome-devtools-mcp package.
update-chrome-devtools-mcp-package:
  nix-update --flake chrome-devtools-mcp --version-regex 'chrome-devtools-mcp-v(.*)'

# Update the codex-bin package.
update-codex-bin-package:
  ./pkgs/codex-bin/update.sh

# Update the claude-code-bin package.
update-claude-code-bin-package:
  ./pkgs/claude-code-bin/update.sh

# Update the nvim-rs package.
update-nvim-package:
  ./pkgs/nvim-rs/update.sh

# Update all custom packages.
update-packages:
  @just update-chrome-devtools-mcp-package
  @just update-codex-bin-package
  @just update-claude-code-bin-package
  @just update-nvim-package

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
