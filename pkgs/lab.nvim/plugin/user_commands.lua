-- Lazy-load commands: modules are only required when commands are invoked
vim.api.nvim_create_user_command('Node', function()
  require('editor.repl.node').open()
end, { desc = 'Open a NodeJS repl', force = true })

vim.api.nvim_create_user_command('Nix', function()
  require('editor.repl.nix').open()
end, { desc = 'Open a Nix repl', force = true })

vim.api.nvim_create_user_command('Permissions', function(opts)
  require('editor.perms').command(opts)
end, { desc = 'Manage file permissions', force = true, nargs = '?' })

vim.api.nvim_create_user_command('Author', function(opts)
  require('git.author').command(opts.line1, opts.line2)
end, { desc = 'Show git blame authorship', force = true, range = true })
