---
description: Neovim plugin development guidance, custom framework API, and editor conventions. Use when working on neovim plugins, editing .vimrc.lua, or modifying the editor platform.
---

# Developing Neovim Plugins

- Use `nvim --headless -c 'help <name> | echo expand("%:p") | qa'` to find plugin help pages.
- Use `nvim --headless -c 'echo $VIMRUNTIME | qa'` to find the neovim runtime.
- When editing `.vimrc.lua`, see the framework guide below.

# Neovim Framework

Custom plugin loader via `require('core.pkg')`. Used in `.vimrc.lua` files.

## API

```lua
-- Add a plugin
pkg.add(name, { type = 'path', source = '...', config? = function(opts) end, opts? = {} })

-- Override/replace a plugin (receives nil if not found)
pkg.override(name, function(plugin) return modified_plugin end)

-- Query plugins
pkg.find(name, 'active'|'bundled') -> plugin|nil
pkg.list_active() -> plugin[]
pkg.list_bundled() -> plugin[]
```

## Example

```lua
-- .vimrc.lua: use local fork of a plugin
local repo = vim.fn.expand('<sfile>:h')

require('core.pkg').override('my-plugin', function(plugin)
  return vim.tbl_extend('force', plugin or {}, {
    source = repo,
    type = 'path',
  })
end)
```
