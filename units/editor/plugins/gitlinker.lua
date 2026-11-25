local gitlinker = require('gitlinker')

gitlinker.setup({
  -- mappings = nil,
})

vim.keymap.set('n', '<leader>l', function()
  gitlinker.get_buf_range_url(vim.fn.mode())
end)

-- TODO: Figure out why this doesn't work with `keymap.set(...)`.
vim.api.nvim_set_keymap(
  'v',
  '<leader>l',
  "<cmd>lua require'gitlinker'.get_buf_range_url('v')<cr>",
  {
    noremap = true,
  }
)
