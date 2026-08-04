return function(config)
  config.format_on_save = function(bufnr)
    if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
      return
    end

    return {}
  end

  require('conform').setup(config)

  vim.api.nvim_create_user_command('FormatDisable', function(opts)
    if opts.args == 'global' then
      vim.g.disable_autoformat = true
      vim.notify('Format on save disabled (global)')
    else
      vim.b.disable_autoformat = true
      vim.notify('Format on save disabled (buffer)')
    end
  end, {
    desc = 'Disable format on save',
    nargs = '?',
    complete = function()
      return { 'buffer', 'global' }
    end,
  })

  vim.api.nvim_create_user_command('FormatEnable', function(opts)
    if opts.args == 'global' then
      vim.g.disable_autoformat = nil
      vim.notify('Format on save enabled (global)')
    else
      vim.b.disable_autoformat = nil
      vim.notify('Format on save enabled (buffer)')
    end
  end, {
    desc = 'Enable format on save',
    nargs = '?',
    complete = function()
      return { 'buffer', 'global' }
    end,
  })
end
