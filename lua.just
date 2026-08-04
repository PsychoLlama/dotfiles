set default-list
set indentation := "  "

# Run luacheck on all Lua files.
lint:
  luacheck pkgs modules

# Run Lua unit tests with vusted.
test:
  vusted pkgs modules

# Run lua-language-server type checks.
typecheck:
  #!/usr/bin/env bash
  set -euo pipefail
  export VIMRUNTIME=$(nvim --clean --headless --cmd 'echo $VIMRUNTIME | q' 2>&1)
  lua-language-server --check . --checklevel=Warning
