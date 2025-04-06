--- @brief
--- A minimal plugin loader built around native packages. Plugins are expected
--- to exist in `&packpath` as optional dependencies. This module decides if,
--- when, and how to load them.

local loader = require('core.pkg._loader')

--- Neovim standard library. It's safe to assume this is always loaded, but
--- the order of `packadd` and `runtimepath` is important. The standard
--- library must always appear first.
---
--- @type core.pkg.Plugin
local stdlib = {
  type = 'path',
  source = vim.env.VIMRUNTIME,
  name = 'VIMRUNTIME',
  opts = {},
}

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

--- @alias core.pkg.Hook fun(plugins: core.pkg.Plugin[]): nil|core.pkg.Plugin[]
--- @type core.pkg.Hook[]
local hooks = {}

local M = {}

--- Load all plugins. Optionally, pass a callback to change the list of
--- plugins before loading.
--- @return core.pkg.Plugin[]
function M.load()
  local plugins = loader.get_manifest()

  for _, plugin_hook in ipairs(hooks) do
    plugins = plugin_hook(plugins) or plugins
  end

  for _, plugin in ipairs(plugins) do
    load_plugin(plugin)
    table.insert(loaded_plugins, plugin)
  end

  -- Idempotent operation. This ensures the standard libary evaluates first.
  load_plugin(stdlib)

  return loaded_plugins
end

--- Register a hook to preprocess the list of plugins before loading. Useful
--- for dynamically adding, replacing, or removing plugins from the manifest.
--- MUST be called before `load()`.
--- @param hook core.pkg.Hook
function M.add_hook(hook)
  table.insert(hooks, hook)
end

--- Overrides a plugin matched by `name`. Must be called before `load()`.
--- Called with `nil` if no existing plugin exists.
--- @param name string Name of the plugin to add or replace.
--- @param replace fun(plugin: nil|core.pkg.Plugin): core.pkg.Plugin
function M.override(name, replace)
  M.add_hook(function(plugins)
    -- Default to adding the plugin if it doesn't exist.
    local plugin_index = #plugins + 1

    for i, plugin in ipairs(plugins) do
      if plugin.name == name then
        plugin_index = i
        break
      end
    end

    local plugin = replace(plugins[plugin_index])
    if plugin then
      plugins[plugin_index] = plugin
    end

    return plugins
  end)
end

--- Get all plugins from the manifest. Includes inactive plugins. Does not
--- include custom plugins provided by the optional override callback.
--- @return core.pkg.Plugin[]
function M.list_available()
  return loader.get_manifest()
end

--- Get all plugins that have been loaded, including any custom plugins.
--- @return core.pkg.Plugin[]
function M.list_active()
  return loaded_plugins
end

--- Get all hooks that have been registered.
--- @return core.pkg.Hook[]
function M.list_hooks()
  return vim.deepcopy(hooks)
end

--- Alias of `override()`. Kept for backwards compat with `.vimrc.lua`.
--- @derepcated Use `override` instead.
--- @param override core.pkg.Hook
function M.on_load(override)
  return M.add_hook(override)
end

return M
