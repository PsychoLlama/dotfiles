local M = {}

function M.open(opts)
  vim.cmd.new(opts.title)

  if opts.cwd then
    vim.cmd.lcd(vim.fn.fnameescape(opts.cwd))
  end

  vim.cmd.wincmd('J')
  vim.cmd.resize(10)
  vim.opt_local.number = false
  vim.opt_local.signcolumn = 'no'
  vim.fn.termopen(opts.command)
  vim.cmd.normal('A')
end

return M
