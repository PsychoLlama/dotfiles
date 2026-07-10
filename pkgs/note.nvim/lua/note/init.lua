local config = require('note.config')
local notes = require('note.notes')

local M = {}

-- Re-export the note actions as a plain API, e.g. `require('note').create`.
M.open = notes.open
M.create = notes.create

--- Set up note.nvim: store the config and register keymaps.
--- @param opts { path: string } The slip box directory (required).
function M.setup(opts)
  config.set(opts)

  vim.keymap.set('n', '<leader>no', M.open, { desc = 'Open notes' })
  vim.keymap.set('n', '<leader>ni', M.create, { desc = 'Create note' })
end

return M
