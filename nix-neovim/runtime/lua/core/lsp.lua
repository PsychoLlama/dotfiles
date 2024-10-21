--- @brief
--- A Lua plugin for managing LSP clients. This is similar to `lspconfig` but
--- leans on newer Neovim features to simplify stuff.


--- Defines a language server and binds it to certain events.
--- @class core.lsp.Server
--- @field name string Identifies the server.
--- @field command string Executable of the server (does not include args).
--- @field filetypes string[] Which files should auto-launch the server.
--- @field root { patterns: string[] } Patterns identifying the project root.
--- @field settings table<string, any> Arbitrary config passed to the server.
---
--- The full set of all supported language servers.
--- @alias core.lsp.Config table<string, core.lsp.Server>


--- @type core.lsp.Config | nil
local global_config = nil

--- @alias core.lsp.StartCallback fun(server: vim.lsp.ClientConfig): nil
--- @type core.lsp.StartCallback[]
local callbacks = {}

local M = {}

--- Configure LSP clients according to file type.
--- @param settings core.lsp.Config
function M.setup(settings)
  global_config = settings

  local filetypes = vim.fn.flatten(
    vim.iter(settings)
    :map(function(_, server) return server.filetypes end)
    :totable()
  )

  vim.api.nvim_create_autocmd('FileType', {
    pattern = filetypes,
    group = vim.api.nvim_create_augroup('core.lsp.launcher', {}),
    callback = function(event)
      vim.iter(settings):each(function(_, server)
        if not vim.tbl_contains(server.filetypes, event.match) then
          return
        end

        -- May be mutated by callbacks.
        local config = {
          name = server.name,
          cmd = server.command,
          root_dir = vim.fs.root(0, server.root.patterns),
          settings = server.settings,
        }

        for _, callback in ipairs(callbacks) do
          callback(server)
        end

        vim.lsp.start(config)
      end)
    end,
  })
end

--- Retrieve the global config for all language servers.
function M.get_config()
  return global_config
end

--- Register a callback to run before a language server starts. Useful for
--- overriding settings or adding additional capabilities.
--- @param callback core.lsp.StartCallback
function M.on_start(callback)
  table.insert(callbacks, callback)
end

return M
