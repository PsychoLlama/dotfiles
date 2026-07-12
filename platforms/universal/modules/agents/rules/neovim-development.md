---
paths:
  - "**/*.lua"
---

# Developing Neovim Plugins

Discover APIs from the running editor and annotated source. Do not trust memorized signatures; they drift.

## Discover Neovim APIs

- Help page path: `nvim --headless -c 'help <name> | echo expand("%:p") | qa'`.
- Runtime path: `nvim --headless -c 'echo $VIMRUNTIME | qa'`.
- Neovim version: `nvim --version | head -1`. Target the installed version's API (Lua `vim.*`, not deprecated shims).
- Read a plugin's `lua/` and `doc/` directories under the runtime path for its real surface.
