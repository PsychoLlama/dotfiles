local repo = vim.fn.expand('<sfile>:h')

require('core.pkg').on_load(function(plugins)
  return vim
    .iter(plugins)
    :map(function(plugin)
      if plugin.name ~= 'ext.nvim' then
        return plugin
      end

      return vim.tbl_extend('force', plugin, {
        type = 'path',
        source = vim.fs.joinpath(repo, 'pkgs/ext.nvim'),
      })
    end)
    :totable()
end)
