vim.api.nvim_set_keymap(
  'n',
  '<leader>no',
  ':lua require("regal").open_notes()<cr>',
  { noremap = true, silent = true }
)

vim.api.nvim_set_keymap(
  'n',
  '<leader>ni',
  ':lua require("regal").create_note()<cr>',
  { noremap = true, silent = true }
)
