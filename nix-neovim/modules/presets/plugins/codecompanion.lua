local companion = require('codecompanion')

companion.setup()

vim.keymap.set('n', '<leader>c', function()
  companion.toggle()

  if vim.opt.filetype:get() == 'codecompanion' then
    vim.opt_local.number = false
    vim.opt_local.signcolumn = 'no'
    vim.api.nvim_feedkeys('i', 'n', true)
  end
end)
