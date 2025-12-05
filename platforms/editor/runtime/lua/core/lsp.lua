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

--- @type core.lsp.Config
local servers = {}

--- @alias core.lsp.AttachCallback fun(config: vim.lsp.ClientConfig): nil
--- @type core.lsp.AttachCallback[]
local callbacks = {}

local M = {}

--- @param filetype string
local function launch_servers(filetype)
  vim.iter(servers):each(function(_, server)
    if not vim.tbl_contains(server.filetypes, filetype) then
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
      callback(config)
    end

    vim.lsp.start(config)
  end)
end

--- Configure LSP clients according to file type.
--- @param settings core.lsp.Config
function M.setup(settings)
  servers = settings

  vim.api.nvim_create_autocmd('FileType', {
    pattern = '*',
    group = vim.api.nvim_create_augroup('core.lsp.launcher', {}),
    callback = function(event)
      launch_servers(event.match)
    end,
  })
end

--- Add a language server dynamically after setup.
--- @param server core.lsp.Server
function M.add(server)
  servers[server.name] = server
  launch_servers(vim.bo.filetype)
end

--- Retrieve the global config for all language servers.
function M.get_config()
  return servers
end

--- Register a callback to run before a language server starts. Useful for
--- overriding settings or adding additional capabilities.
--- @param callback core.lsp.AttachCallback
function M.on_attach(callback)
  table.insert(callbacks, callback)
end

return M
