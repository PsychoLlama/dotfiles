local actions = require('telescope.actions')
local telescope = require('telescope')

telescope.setup({
  defaults = {
    path_display = { 'filename_first' },
    layout_strategy = 'vertical',
    layout_config = {
      height = { padding = 0 },
      width = { padding = 0 },
    },
    mappings = {
      n = {
        ['<c-c>'] = actions.close,
      },
    },
  },
})

vim.keymap.set('n', '<c-space>', function()
  require('telescope.builtin').builtin({
    include_extensions = true,
  })
end)

vim.keymap.set('n', '<leader>f', function()
  require('telescope.builtin').find_files()
end)

vim.keymap.set('n', '<leader>b', function()
  require('telescope.builtin').buffers()
end)
