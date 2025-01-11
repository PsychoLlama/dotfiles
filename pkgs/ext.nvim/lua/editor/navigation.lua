local M = {}

-- Open the containing project. Can be repeated (useful in monorepos).
function M.open_project_root()
  local current_dir = vim.fs.normalize(vim.fn.expand('%:p'))
  local parent_dir = vim.fn.fnamemodify(current_dir, ':h')
  local root = vim.fs.root(parent_dir, {
    '.git/',
    'package.json',
    'Cargo.toml',
  })

  if root then
    require('navitron').open(root)
  else
    vim.cmd.echohl('Error')
    vim.cmd.echon('"Error:"')
    vim.cmd.echohl('Clear')
    vim.cmd.echon('" No project root above this."')
  end
end

return M
