local lualine_theme = require('lualine.themes.onedark')

lualine_theme.normal.c.bg = nil
lualine_theme.inactive.c.bg = nil

-- Disable branch name mode colors.
lualine_theme.normal.b.fg = nil
lualine_theme.insert.b.fg = nil
lualine_theme.visual.b.fg = nil
lualine_theme.command.b.fg = nil

require('lualine').setup({
  options = {
    theme = lualine_theme,
  },
  sections = {
    lualine_a = {},
    lualine_b = { 'branch' },
    lualine_c = { 'filename', 'diagnostics' },
    lualine_x = { 'lsp_progress', 'filetype' },
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
