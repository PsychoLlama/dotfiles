local config = require('note.config')
local notes = require('note.notes')

local M = {}

-- Re-export the note actions as a plain API, e.g. `require('note').create`.
M.open = notes.open
M.create = notes.create
M.rename = notes.rename

--- Set up note.nvim: store the config and register keymaps.
--- @param opts { path: string } The slip box directory (required).
function M.setup(opts)
  config.set(opts)

  local conf = config.get()
  if not conf then
    return
  end

  vim.keymap.set('n', '<leader>no', M.open, { desc = 'Open notes' })
  vim.keymap.set('n', '<leader>ni', M.create, { desc = 'Create note' })

  -- Rename is only meaningful inside the slip box, so bind it buffer-locally
  -- to notes rather than globally.
  vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
    group = vim.api.nvim_create_augroup('note.nvim', { clear = true }),
    pattern = conf.path .. '/*',
    desc = 'Enable the note rename keymap inside the slip box',
    callback = function(args)
      vim.keymap.set('n', '<leader>nr', M.rename, {
        buffer = args.buf,
        desc = 'Rename note',
      })
    end,
  })
end

return M
