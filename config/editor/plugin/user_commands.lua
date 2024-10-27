vim.api.nvim_create_user_command(
  'Node',
  require('editor.repl.node').open,
  { desc = 'Open a NodeJS repl', force = true }
)

vim.api.nvim_create_user_command(
  'Nix',
  require('editor.repl.nix').open,
  { desc = 'Open a Nix repl', force = true }
)

vim.api.nvim_create_user_command(
  'Permissions',
  require('editor.perms').command,
  { desc = 'Manage file permissions', force = true, nargs = '?' }
)
