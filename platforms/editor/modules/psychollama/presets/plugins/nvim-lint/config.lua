return function(config)
  local nvim_lint = require('lint')

  nvim_lint.linters_by_ft = config.linters_by_ft or {}

  -- Merging linters individually. The metatable prevents a clean deep merge.
  for name, linter_config in pairs(config.linters or {}) do
    nvim_lint.linters[name] =
      vim.tbl_extend('force', nvim_lint.linters[name], linter_config)
  end

  vim.api.nvim_create_autocmd({
    'BufWinEnter',
    'InsertLeave',
    'TextChanged',
  }, {
    group = vim.api.nvim_create_augroup('lab.autolint', {}),
    desc = 'Run standalone linters',
    callback = function()
      -- TODO: Throttle this. `TextChanged` can happen a lot.
      nvim_lint.try_lint()
    end,
  })
end
