-- Disable the `-- INSERT --` text. It's owned by lualine.
vim.o.showmode = false

local lualine_theme = require('lualine.themes.onedark')

local function invert_colors(m)
  m.fg, m.bg = m.bg, m.fg
end

-- Customize the theme.
lualine_theme.inactive.c.bg = nil
invert_colors(lualine_theme.normal.b)
invert_colors(lualine_theme.insert.b)
invert_colors(lualine_theme.visual.b)
invert_colors(lualine_theme.command.b)

require('lualine').setup({
  options = {
    theme = lualine_theme,
  },
  sections = {
    lualine_a = {},
    lualine_b = { 'mode' },
    lualine_c = { 'filename', 'diagnostics' },
    lualine_x = { vim.ui.progress_status, 'filetype' },
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
