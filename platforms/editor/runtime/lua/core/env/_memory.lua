local M = {}

--- Get the file where we store permission memory.
--- @return string
function M.get_file()
  local state_dir = vim.fn.stdpath('state')

  --- @cast state_dir string
  return vim.fs.joinpath(state_dir, 'dynamic-sources.json')
end

--- @alias core.env.PermissionMemory table<string, string>
---
--- Get the directory where we store permission memory.
--- @return core.env.PermissionMemory
function M.load()
  local file = io.open(M.get_file(), 'r')
  if not file then
    return {}
  end

  local content = file:read('*a')
  file:close()

  return vim.json.decode(content)
end

--- Save the memory file.
--- @param settings core.env.PermissionMemory
function M.save(settings)
  local file = io.open(M.get_file(), 'w')
  if not file then
    vim.notify('core.env: Could not save memory file', vim.log.levels.WARN)
    return
  end

  file:write(vim.json.encode(settings))
  file:close()
end

--- Remember a permission for a file.
--- @param file_path string
--- @param permission core.env.Permission
function M.update_permission(file_path, permission)
  local settings = M.load()
  settings[file_path] = permission
  M.save(settings)
end

--- Check if we've approved or denied this file before.
--- @return core.env.Permission
function M.get_permission(file_path)
  local settings = M.load()
  return settings[file_path] or 'unknown'
end

return M
