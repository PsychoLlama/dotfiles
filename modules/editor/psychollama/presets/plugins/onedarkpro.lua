-- Color scheme (VS Code's One Dark Pro theme)
local odp = require('onedarkpro')
local odp_helpers = require('onedarkpro.helpers')
local colors = odp_helpers.get_colors('onedark')
local bg_color = '#1e1e1e'

odp.setup({
  options = {
    transparency = true,
  },
  colors = {
    onedark = {
      bg = bg_color,
    },
  },
  highlights = {
    -- Editor built-ins. See `:h highlight-groups` for details.
    CursorLineNr = {
      fg = colors.blue,
    },

    FloatTitle = {
      bg = bg_color,
      fg = colors.blue,
    },
  },
  styles = {
    comments = 'italic',
  },
})

odp.load()

-- Clear the cursor line. I only use it for the cursor line number.
vim.api.nvim_set_hl(0, 'CursorLine', {})

vim.api.nvim_set_hl(0, 'FloatBorder', {
  bg = bg_color,
  fg = colors.black,
  blend = 20,
})

vim.api.nvim_set_hl(0, 'FloatTitle', {
  fg = colors.blue,
  bg = bg_color,
  blend = 20,
})

vim.api.nvim_set_hl(0, 'NormalFloat', {
  bg = bg_color,
  blend = 20,
})

-- nvim-cmp selected option.
vim.api.nvim_set_hl(0, 'PmenuSel', {
  bg = odp_helpers.lighten('bg', 10, 'onedark'),
  fg = 'NONE',
})
