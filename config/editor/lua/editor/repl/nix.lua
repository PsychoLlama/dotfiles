local M = {}

function M.open()
  require('editor.repl').open({
    title = 'Nix Repl',
    command = 'nix repl -f flake:nixpkgs --offline',
  })
end

return M
