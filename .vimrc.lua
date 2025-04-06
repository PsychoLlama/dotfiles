local repo = vim.fn.expand('<sfile>:h')

require('core.pkg').override('ext.nvim', function(plugin)
  return vim.tbl_extend('force', plugin or {}, {
    source = vim.fs.joinpath(repo, 'pkgs/ext.nvim'),
    type = 'path',
  })
end)
