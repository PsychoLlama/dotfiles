local config = require('regal.config')
local notes = require('regal.notes')

return {
  setup = function(opts)
    config.slip_box = vim.fs.normalize(opts.slip_box)

    vim.api.nvim_set_keymap('n', '<Plug>(regal-open-notes)', '', {
      callback = notes.open,
      silent = true,
      noremap = true,
    })

    vim.api.nvim_set_keymap('n', '<Plug>(regal-create-note)', '', {
      callback = notes.create,
      silent = true,
      noremap = true,
    })
  end,
}
