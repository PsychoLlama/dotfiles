---
paths:
  - "**/.vimrc.lua"
---

# `.vimrc.lua` and the `core` framework

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
