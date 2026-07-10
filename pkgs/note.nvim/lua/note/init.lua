local config = require('note.config')
local notes = require('note.notes')

local M = {}

-- Re-export the note actions as a plain API, e.g. `require('note').create`.
M.open = notes.open
M.create = notes.create
M.rename = notes.rename

--- Configure note.nvim. Keymaps are intentionally left to the caller — the
--- actions are exported above (see the Nix preset for the bindings).
--- @param opts { path: string } The slip box directory (required).
function M.setup(opts)
  config.set(opts)
end

return M
