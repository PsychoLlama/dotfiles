local repo = vim.fn.expand('<sfile>:h')

require('core.pkg').override('lab.nvim', function(plugin)
  return vim.tbl_extend('force', plugin or {}, {
    source = vim.fs.joinpath(repo, 'pkgs/lab.nvim'),
    type = 'path',
  })
end)
