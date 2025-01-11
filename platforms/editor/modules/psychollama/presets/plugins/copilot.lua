vim.g.copilot_no_tab_map = true
vim.api.nvim_set_keymap(
  'i',
  '<c-j>',
  'copilot#Accept("\\<CR>")',
  { noremap = true, expr = true, silent = true }
)
