local M = {}

function M.open()
  if vim.fn.executable('node') == 0 then
    vim.notify('Error: No node executable.', vim.log.levels.error)
    return
  end

  vim.cmd.new("Node Repl")
  vim.cmd.wincmd('J')
  vim.cmd.resize(10)
  vim.cmd.lcd(vim.fn.fnameescape(M.get_project()))
  vim.opt_local.number = false
  vim.opt_local.signcolumn = "no"
  vim.fn.termopen('yarn node')
  vim.cmd.normal('A')
end

function M.get_project()
  return vim.fs.root(0, { 'package.json' }) or vim.env.PWD
end

return M
