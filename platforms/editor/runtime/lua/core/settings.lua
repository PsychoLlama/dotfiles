local M = {}

--- Set global neovim options.
--- @param options table<string, any>
function M.apply(options)
  for key, value in pairs(options) do
    vim.opt[key] = value
  end
end

return M
