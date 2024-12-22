local M = {}

--- Set global neovim options.
--- @param options table<string, any>
function M.set(options)
  for key, value in pairs(options) do
    vim.opt[key] = value
  end
end

return M
