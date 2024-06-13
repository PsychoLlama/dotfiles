vim.api.nvim_create_user_command(
  'Node',
  require('editor.repl.node').open,
  { desc = "Open a NodeJS repl", force = true }
)
