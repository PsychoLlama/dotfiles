local M = {}

--- @class core.pkg.Plugin
--- @field type 'pack' | 'path' Loading strategy
--- @field name string Unique plugin name
--- @field source string Path to the plugin's source code
--- @field config nil | string | core.pkg.ConfigHook Optional config file or function
--- @field opts table Given options if the config returns a function
--- @field defer nil | core.pkg.DeferSpec Lazy-loading triggers
--- @type core.pkg.Plugin[]
local plugin_manifest = {}

--- Set the plugin manifest
--- @param manifest core.pkg.Plugin[]
function M.set_manifest(manifest)
  plugin_manifest = manifest
end

--- Get the plugin manifest
function M.get_manifest()
  return plugin_manifest
end

--- Get only non-deferred plugins from the manifest
function M.get_eager_plugins()
  local eager = {}
  for _, plugin in ipairs(plugin_manifest) do
    if not plugin.defer then
      table.insert(eager, plugin)
    end
  end
  return eager
end

--- Get only deferred plugins from the manifest
function M.get_deferred_plugins()
  local deferred = {}
  for _, plugin in ipairs(plugin_manifest) do
    if plugin.defer then
      table.insert(deferred, plugin)
    end
  end
  return deferred
end

--- @alias core.pkg.ConfigHook fun(opts: table): unknown
--- Load config files for all active plugins (non-deferred only).
function M.eval_configs()
  local plugins = require('core.pkg').list_active()

  for _, plugin in ipairs(plugins) do
    -- Skip deferred plugins - their configs run when they load
    if not plugin.defer and plugin.config then
      --- @type string | core.pkg.ConfigHook
      local callback = plugin.config

      if type(callback) == 'string' then
        callback = dofile(callback)
      end

      if type(callback) == 'function' then
        callback(plugin.opts)
      end
    end
  end
end

--- Register deferred plugins with the defer module
function M.register_deferred()
  local deferred = M.get_deferred_plugins()

  -- Only require the defer module if there are deferred plugins
  if #deferred == 0 then
    return
  end

  local defer = require('core.pkg.defer')
  for _, plugin in ipairs(deferred) do
    defer.register(plugin, plugin.defer)
  end
end

return M
