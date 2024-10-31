return function(opts)
  local companion = require('codecompanion')

  companion.setup(opts)

  vim.cmd.cabbrev('cc', 'CodeCompanion')
  vim.keymap.set('n', '<leader>c', companion.toggle, {
    desc = 'Toggle CodeCompanion',
  })

  vim.api.nvim_create_autocmd('FileType', {
    group = vim.api.nvim_create_augroup('codecompanion', {}),
    pattern = 'codecompanion',
    callback = vim.schedule_wrap(function()
      vim.opt_local.number = false
      vim.opt_local.signcolumn = 'yes'
      vim.api.nvim_feedkeys('i', 'n', true)
    end),
  })
end
