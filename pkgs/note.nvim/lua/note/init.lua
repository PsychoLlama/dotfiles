local config = require('note.config')
local notes = require('note.notes')

return {
  setup = function(opts)
    config.slip_box = vim.fs.normalize(opts.slip_box)

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
  end,
}
