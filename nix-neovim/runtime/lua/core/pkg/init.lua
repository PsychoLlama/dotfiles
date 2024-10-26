--- @brief
--- A minimal plugin loader built around native packages. Plugins are expected
--- to exist in `&packpath` as optional dependencies. This module decides if,
--- when, and how to load them.

local loader = require('core.pkg._loader')

--- @param plugin core.pkg.Plugin
local function load_plugin(plugin)
  if plugin.type == 'pack' then
    vim.cmd.packadd(plugin.name)
  else
    vim.opt.runtimepath:prepend(plugin.source)
    local after = vim.fs.joinpath(plugin.source, 'after')

    if vim.fn.isdirectory(after) == 1 then
      vim.opt.runtimepath:append(after)
    end
  end
end

--- @type core.pkg.Plugin[]
local loaded_plugins = {}

local M = {}

--- Load all plugins. Optionally, pass a callback to change the list of
--- plugins before loading.
--- @param override nil | fun(manifest: core.pkg.Plugin[]): core.pkg.Plugin[]
function M.load(override)
  local plugins = loader.get_manifest()

  if override then
    plugins = override(plugins)
  end

  for _, plugin in ipairs(plugins) do
    load_plugin(plugin)
    table.insert(loaded_plugins, plugin)
  end

  return loaded_plugins
end

--- Get all plugins from the manifest. Includes inactive plugins. Does not
--- include custom plugins provided by the optional override callback.
function M.list_available()
  return loader.get_manifest()
end

--- Get all plugins that have been loaded, including any custom plugins.
function M.list_active()
  return loaded_plugins
end

return M
