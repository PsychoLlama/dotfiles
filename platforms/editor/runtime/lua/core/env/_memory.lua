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

--- Iterate a path and each of its ancestor directories, nearest first.
--- @param path string An absolute path.
--- @return fun(): string|nil
local function ancestors(path)
  -- Normalize away trailing slashes and `.`/`..` segments so directory keys
  -- compare consistently regardless of how the path was spelled.
  local current = vim.fs.normalize(path)
  local done = false

  return function()
    if done then
      return nil
    end

    local result = current
    local parent = vim.fs.dirname(current)

    -- `dirname` is idempotent at the filesystem root; stop once it stops
    -- climbing to avoid looping forever.
    if parent == current then
      done = true
    else
      current = parent
    end

    return result
  end
end

--- Remember a permission for a directory. Trust is granted to the directory
--- containing a vimrc, so worktrees and other files beneath it inherit it.
--- @param dir string The directory to trust or block.
--- @param permission core.env.Permission
function M.update_permission(dir, permission)
  local settings = M.load()
  settings[vim.fs.normalize(dir)] = permission
  M.save(settings)
end

--- Check whether a directory (or any ancestor) has a remembered permission.
--- The nearest, most-specific ancestor wins, so a blocked subdirectory can
--- override a trusted parent.
--- @param dir string The directory to resolve a permission for.
--- @return core.env.Permission
function M.get_permission(dir)
  local settings = M.load()

  for ancestor in ancestors(dir) do
    if settings[ancestor] then
      return settings[ancestor]
    end
  end

  return 'unknown'
end

return M
