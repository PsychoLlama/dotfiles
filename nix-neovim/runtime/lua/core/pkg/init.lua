--- @brief
--- A minimal plugin loader built around native packages. Plugins are expected
--- to exist in `&packpath` as optional dependencies. This module decides if,
--- when, and how to load them.

local manifest = require('core.pkg._manifest')

--- @type core.pkg.Plugin[]
local loaded_plugins = {}

local M = {}

--- Load all plugins. Optionally, pass a callback to change the list of
--- plugins before loading.
--- @param override nil | fun(manifest: core.pkg.Plugin[]): core.pkg.Plugin[]
function M.load(override)
  local plugins = manifest.get()

  if override then
    plugins = override(plugins)
  end

  for _, plugin in ipairs(plugins) do
    vim.cmd.packadd(plugin.name)
    table.insert(loaded_plugins, plugin)
  end

  return loaded_plugins
end

--- Get all plugins from the manifest, even if they are not loaded.
function M.get_all()
  return manifest.get()
end

--- Get all plugins that have been loaded.
function M.get_activeget_active()
  return loaded_plugins
end

return M
