local memory = require('core.env._memory')

local M = {}

--- @alias core.env.Permission 'allow' | 'deny' | 'unknown'
---
--- Check if we're allowed to source a file. If we don't have a saved
--- preference, ask permission and remember for next time.
---
--- Trust is tracked per directory rather than per file: granting it to the
--- directory containing a vimrc lets anything beneath inherit it (e.g. git
--- worktrees that share the same project root).
--- @param file_path string The file to source.
--- @param opts core.env.PromptOptions
--- @return core.env.Permission
function M.check_memory_or_ask(file_path, opts)
  local dir = vim.fs.dirname(file_path)
  local permission = memory.get_permission(dir)

  if vim.tbl_contains({ 'allow', 'deny' }, permission) then
    return permission
  end

  permission = M.ask(file_path, opts)
  memory.update_permission(dir, permission)

  return permission
end

--- @class core.env.PromptOptions
--- @field prompt nil|string Custom prompt label.
---
--- Check if we're allowed to source a file.
--- @param file_path string The file to source.
--- @param opts core.env.PromptOptions
function M.ask(file_path, opts)
  opts = opts or {}
  opts.prompt = opts.prompt or 'Trust extra vimrc?'

  -- Allowing trusts the containing directory, so make that explicit: every
  -- file beneath it will be sourced without another prompt.
  local dir = vim.fn.fnamemodify(file_path, ':h:~:.')

  -- Load order is important. Must be a blocking operation.
  local selection = vim.fn.inputlist({
    opts.prompt,
    ('1: Allow directory: %s'):format(dir),
    '2: Block',
  })

  if selection == 1 then
    return 'allow'
  end

  if selection == 2 then
    return 'deny'
  end

  return 'unknown'
end

return M
