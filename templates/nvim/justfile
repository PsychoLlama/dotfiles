_:
  just --list

# Format all code.
fmt:
  treefmt

# Check formatting.
fmt-check:
  treefmt --ci

# Run luacheck on all Lua files.
lint:
  luacheck lua

# Run Lua unit tests with vusted.
unit-test:
  vusted lua

# Run lua-language-server type checks.
typecheck:
  #!/usr/bin/env bash
  set -euo pipefail
  export VIMRUNTIME=$(nvim --clean --headless --cmd 'echo $VIMRUNTIME | q' 2>&1)
  lua-language-server --check . --checklevel=Warning

# Run all checks (lint + typecheck + unit tests), reporting all failures.
check:
  #!/usr/bin/env bash
  failed=0
  just lint || failed=1
  just typecheck || failed=1
  just unit-test || failed=1
  exit $failed
