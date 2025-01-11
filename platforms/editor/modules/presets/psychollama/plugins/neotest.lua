return function()
  local neotest = require('neotest')

  neotest.setup({
    adapters = {
      require('neotest-vitest'),
    },
  })

  -- Leaves signcolumn junk after a run. Haven't found a workaround yet.
  vim.keymap.set('n', '<leader>;', function()
    neotest.watch.toggle(vim.fn.expand('%:p'))
  end, { desc = 'Run unit tests' })
end
