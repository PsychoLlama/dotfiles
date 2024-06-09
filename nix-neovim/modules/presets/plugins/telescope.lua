local telescope = require('telescope')
local actions = require('telescope.actions')

telescope.setup({
  defaults = {
    path_display = { "filename_first" },
    layout_strategy = 'vertical',
    layout_config = {
      height = { padding = 0 },
      width = { padding = 0 },
    },
    mappings = {
      n = {
        ["<c-c>"] = actions.close,
      }
    },
  },
})

vim.keymap.set('n', '<c-space>', function()
  require('telescope.builtin').builtin({
    include_extensions = true,
  })
end)
