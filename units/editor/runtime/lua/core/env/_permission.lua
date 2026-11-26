local memory = require('core.env._memory')

local M = {}

--- Directories trusted ahead of time, seeded declaratively from Nix. Anything
--- at or beneath one is allowed without a prompt, and the decision is never
--- written to memory -- configuration is the source of truth.
--- @type string[]
M.trusted_prefixes = {}

--- Replace the set of trusted prefixes. Paths are normalized so `~` expansion
--- and trailing slashes line up with resolved vimrc directories.
--- @param prefixes string[]
function M.set_trusted_prefixes(prefixes)
  M.trusted_prefixes = vim.tbl_map(vim.fs.normalize, prefixes)
end

--- Whether a directory sits at or beneath a configured trusted prefix.
--- @param dir string A normalized directory.
--- @return boolean
function M.is_trusted_prefix(dir)
  for _, prefix in ipairs(M.trusted_prefixes) do
    if dir == prefix or vim.startswith(dir, prefix .. '/') then
      return true
    end
  end

  return false
end

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
  local dir = vim.fs.normalize(vim.fs.dirname(file_path))

  -- Declarative trust wins outright: never prompt, never persist.
  if M.is_trusted_prefix(dir) then
    return 'allow'
  end

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
