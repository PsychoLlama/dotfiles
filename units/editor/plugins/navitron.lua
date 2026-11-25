local navitron = require('navitron')

navitron.setup({
  -- Default options
})

vim.api.nvim_set_keymap('n', '<leader>[', '', {
  noremap = true,
  silent = true,
  desc = 'Explore parent directory',
  callback = function()
    if vim.tbl_contains({ 'navitron', 'netrw' }, vim.o.filetype) then
      return
    end

    local current_file = vim.fn.expand('%:p')
    local parent_directory = vim.fn.fnamemodify(current_file, ':p:h')
    navitron.open(parent_directory)
  end,
})
