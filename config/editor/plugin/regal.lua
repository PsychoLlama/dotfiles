vim.api.nvim_set_keymap(
  'n',
  '<leader>no',
  '<Plug>(regal-open-notes)',
  { noremap = true }
)

vim.api.nvim_set_keymap(
  'n',
  '<leader>ni',
  '<Plug>(regal-create-note)',
  { noremap = true }
)
