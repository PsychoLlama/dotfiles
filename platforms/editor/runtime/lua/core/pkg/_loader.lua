local M = {}

--- @class core.pkg.Plugin
--- @field type 'pack' | 'path' Loading strategy
--- @field name string Unique plugin name
--- @field source string Path to the plugin's source code
--- @field config nil | string | core.pkg.ConfigHook Optional config file or function
--- @field opts table Given options if the config returns a function
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

--- @alias core.pkg.ConfigHook fun(opts: table): unknown
--- Load config files for all active plugins.
function M.eval_configs()
  local plugins = require('core.pkg').list_active()

  for _, plugin in ipairs(plugins) do
    if plugin.config then
      --- @type core.pkg.ConfigHook
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

return M
