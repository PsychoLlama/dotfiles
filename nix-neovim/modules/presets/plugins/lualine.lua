local lualine_theme = require('lualine.themes.onedark')

lualine_theme.normal.c.bg = nil

pcall(function()
  -- Disable fg mode colors for the branch name.
  -- Unknown why, but sometimes these objects don't exist yet.
  lualine_theme.normal.b.fg = nil
  lualine_theme.insert.b.fg = nil
  lualine_theme.visual.b.fg = nil
  lualine_theme.command.b.fg = nil
end)

require('lualine').setup({
  options = {
    theme = lualine_theme,
  },
  sections = {
    lualine_a = {},
    lualine_b = { 'branch' },
    lualine_c = { 'filename', 'diagnostics' },
    lualine_x = { 'filetype' },
    lualine_y = { 'progress' },
    lualine_z = { 'location' },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { 'filename' },
    lualine_x = { 'location' },
    lualine_y = {},
    lualine_z = {},
  },
})
