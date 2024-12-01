return function(opts)
  local companion = require('codecompanion')

  -- Merge adapter overrides with the defaults.
  for adapter, config in pairs(opts.adapters or {}) do
    opts.adapters[adapter] =
      require('codecompanion.adapters').extend(adapter, config)
  end

  companion.setup(opts)

  vim.cmd.cabbrev('CC', 'CodeCompanion')
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
