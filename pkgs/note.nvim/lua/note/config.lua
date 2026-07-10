local M = {}

--- @class note.Config
--- @field path string Absolute path to the slip box directory.

--- @type note.Config?
local state

--- Configure the plugin. The slip box `path` is required; it is normalized
--- (`~` expansion, trailing slash and `.`/`..` segments removed) before being
--- stored so callers can spell it however they like. A missing `path` is a
--- misconfiguration, not a crash: warn and leave the config untouched.
--- @param opts { path: string }
function M.set(opts)
  if not (opts and opts.path) then
    vim.notify('note.config: `path` is required', vim.log.levels.WARN)
    return
  end

  state = {
    path = vim.fs.normalize(opts.path),
  }
end

--- Get the current configuration, or `nil` if `set` has not run yet. Warns so
--- callers can bail gracefully instead of the plugin throwing mid-keymap.
--- @return note.Config?
function M.get()
  if not state then
    vim.notify(
      'note.config: not configured; call require("note").setup first',
      vim.log.levels.WARN
    )
  end

  return state
end

return M
