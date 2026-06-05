---
description: Neovim plugin development guidance and the custom `core` startup framework. Use when working on neovim plugins or editing .vimrc.lua.
---

# Developing Neovim Plugins

Discover APIs from the running editor and annotated source. Do not trust memorized signatures; they drift.

## Discover Neovim APIs

- Help page path: `nvim --headless -c 'help <name> | echo expand("%:p") | qa'`.
- Runtime path: `nvim --headless -c 'echo $VIMRUNTIME | qa'`.
- Neovim version: `nvim --version | head -1`. Target the installed version's API (Lua `vim.*`, not deprecated shims).
- Read a plugin's `lua/` and `doc/` directories under the runtime path for its real surface.

## `.vimrc.lua` and the `core` framework

`.vimrc.lua` is a project-local hook that runs arbitrary Lua at Neovim startup.
Use it to do anything on startup — set options, define keymaps and autocommands,
or reshape the plugin manifest. It's the primary consumer of the `core.*`
runtime API.

- Direnv-sourced via `DIRENV_EXTRA_VIMRC`; loaded with a trust prompt by `core.env`.
- Gitignored — never committed.

Read `:help core` before working in this file to understand what the `core.*`
runtime API makes possible (plugin management, startup environment, and the rest).

```bash
nvim --headless -c 'help core | echo expand("%:p") | qa'
```
