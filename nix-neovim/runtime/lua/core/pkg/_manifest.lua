local M = {}

--- @class core.pkg.Plugin
--- @field name string Unique plugin name

--- @type core.pkg.Plugin[]
local plugin_manifest = {}

--- Set the plugin manifest
--- @param manifest core.pkg.Plugin[]
function M.set(manifest)
  plugin_manifest = manifest
end

--- Get the plugin manifest
function M.get()
  return plugin_manifest
end

return M
