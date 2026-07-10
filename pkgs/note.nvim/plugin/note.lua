vim.api.nvim_set_keymap(
  'n',
  '<leader>no',
  '<plug>(note-open-notes)',
  { noremap = true }
)

vim.api.nvim_set_keymap(
  'n',
  '<leader>ni',
  '<plug>(note-create-note)',
  { noremap = true }
)
