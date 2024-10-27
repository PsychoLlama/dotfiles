require('core.pkg').on_load(function(plugins)
  return vim.iter(plugins):map(function(plugin)
    if plugin.name ~= 'personal-vim-config' then
      return plugin
    end

    return vim.tbl_extend('force', plugin, {
      type = 'path',
      source = vim.fs.joinpath(
        vim.fs.root(0, { '.git' }),
        'config/editor'
      ),
    })
  end):totable()
end)
