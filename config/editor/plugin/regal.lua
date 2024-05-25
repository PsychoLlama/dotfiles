vim.api.nvim_set_keymap(
  'n',
  '<leader>no',
  '<plug>(regal-open-notes)',
  { noremap = true }
)

vim.api.nvim_set_keymap(
  'n',
  '<leader>ni',
  '<plug>(regal-create-note)',
  { noremap = true }
)
