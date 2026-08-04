--- @brief
--- Thin wrapper that applies a declarative server manifest through Neovim's
--- built-in `vim.lsp.config()` + `vim.lsp.enable()` APIs. Configs arrive
--- pre-shaped from Nix and are written straight through.

local M = {}

--- Register and enable every server in the manifest.
--- @param servers table<string, vim.lsp.Config>
function M.setup(servers)
  for name, cfg in pairs(servers) do
    vim.lsp.config(name, cfg)
  end

  vim.lsp.enable(vim.tbl_keys(servers))
end

return M
