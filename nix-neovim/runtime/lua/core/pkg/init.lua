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

--- @alias core.pkg.Hook fun(plugins: core.pkg.Plugin[]): core.pkg.Plugin[]
--- @type core.pkg.Hook[]
local hooks = {}

local M = {}

--- Load all plugins. Optionally, pass a callback to change the list of
--- plugins before loading.
function M.load()
  local plugins = loader.get_manifest()

  for _, plugin_look in ipairs(hooks) do
    plugins = plugin_look(plugins)
  end

  for _, plugin in ipairs(plugins) do
    load_plugin(plugin)
    table.insert(loaded_plugins, plugin)
  end

  return loaded_plugins
end

--- Register a hook to preprocess the list of plugins before loading. Useful
--- for dynamically adding, replacing, or removing plugins from the manifest.
function M.on_load(override)
  table.insert(hooks, override)
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

--- Get all hooks that have been registered.
function M.list_hooks()
  return vim.deepcopy(hooks)
end

return M
