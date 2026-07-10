local config = require('note.config')
local notes = require('note.notes')

local M = {}

--- Set up note.nvim and register its `<plug>` mappings.
--- @param opts { path: string } The slip box directory (required).
function M.setup(opts)
  config.set(opts)

  vim.api.nvim_set_keymap('n', '<plug>(note-open-notes)', '', {
    callback = notes.open,
    silent = true,
    noremap = true,
  })

  vim.api.nvim_set_keymap('n', '<plug>(note-create-note)', '', {
    callback = notes.create,
    silent = true,
    noremap = true,
  })
end

return M
