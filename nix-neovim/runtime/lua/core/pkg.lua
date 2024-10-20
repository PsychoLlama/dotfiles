--- @brief
--- A minimal plugin loader built around native packages. Plugins are expected
--- to exist in `&packpath` as optional dependencies. This module decides if,
--- when, and how to load them.
---
--- ==========================================================================
---
--- Packages are loaded with `opt/` instead of `start/` so we can dynamically
--- disable them according to configuration/environment. This has side
--- effects. The most notable is that `start/` packages are implicitly
--- available like `&runtimepath`, whereas lazy packages are only added after
--- calling `:packadd`.
---
--- This poses a challenge because packages must load after the vimrc, but the
--- vimrc may need them ahead of time (e.g. requiring a lua plugin).
---
--- As a workaround, we add lazy packages to the `&runtimepath`, then source
--- the vimrc, then load them correctly.

local M = {}

--- @class core.pkg.Config
--- @field bundle_path string Path to the `opt/` bundle.
--- @field initrc string Path to the `rc/` file.

--- Load the package manifest
--- @param config core.pkg.Config
function M.startup(config)
  -- Create a mapping of all dynamic plugins:
  -- Format: "example-nvim:/path/to/example;another:/path/to/another"
  local dynamic_plugins = {}
  if vim.env.VIM_PLUGINS then
    table.foreach(vim.split(vim.env.VIM_PLUGINS, ';'), function(_, mapping)
      local plugin_name, plugin_path = unpack(vim.split(mapping, ':'))
      dynamic_plugins[plugin_name] = vim.fs.normalize(plugin_path)
    end)
  end

  -- Create a mapping of all static plugins:
  -- { "plugin.vim" = "/plugin/path" }
  local static_plugins = {}
  for plugin_name in vim.fs.dir(config.bundle_path) do
    if not dynamic_plugins[plugin_name] then
      local plugin_path = vim.fs.joinpath(config.bundle_path, plugin_name)
      static_plugins[plugin_name] = plugin_path
    end
  end

  local function export_plugin(plugin_name, plugin_path)
    vim.opt.rtp:prepend(plugin_path)

    local after = vim.fs.joinpath(plugin_path, 'after')
    if vim.fn.isdirectory(after) == 1 then
      vim.opt.rtp:append(after)
    end
  end

  table.foreach(dynamic_plugins, export_plugin)
  table.foreach(static_plugins, export_plugin)

  vim.cmd.source(config.initrc)

  table.foreach(static_plugins, function(plugin_name)
    vim.cmd.packadd(plugin_name)
  end)
end

return M
