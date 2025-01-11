local M = {}

function M.open()
  if vim.fn.executable('node') == 0 then
    vim.notify('Error: No node executable.', vim.log.levels.error)
    return
  end

  require('editor.repl').open({
    title = 'Node Repl',
    command = 'yarn node',
    cwd = vim.fs.root(0, { 'package.json' }) or vim.env.PWD,
  })
end

return M
