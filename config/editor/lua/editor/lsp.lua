-- A Lua plugin for managing LSP clients. This is similar to `lspconfig` but
-- leans on newer Neovim features to simplify stuff. Yarn's Plug'n'Play causes
-- plenty of problems in traditional LSP frameworks.


local M = {
  config = {},
}

--- Configure LSP clients according to file type.
function M.setup(settings)
  M.config = settings

  local filetypes = vim.fn.flatten(
    vim.iter(settings)
    :map(function(_, server) return server.filetypes end)
    :totable()
  )

  vim.api.nvim_create_autocmd('FileType', {
    pattern = filetypes,
    group = vim.api.nvim_create_augroup('lsp_client_setup', {}),
    callback = function(args)
      vim.iter(settings):each(function(_, server)
        if not vim.tbl_contains(server.filetypes, args.match) then
          return
        end

        vim.lsp.start({
          name = server.name,
          cmd = server.command,
          root_dir = vim.fs.root(0, server.root.patterns),
        })
      end)
    end,
  })
end

return M
