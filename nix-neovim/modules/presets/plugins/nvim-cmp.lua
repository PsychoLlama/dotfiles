local cmp = require('cmp')
local core_lsp = require('core.lsp')
local nvim_lsp = require('cmp_nvim_lsp')

local function has_words_before()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local function get_visible_buffers()
  return vim.iter(vim.api.nvim_tabpage_list_wins(0))
      :map(vim.api.nvim_win_get_buf)
      :totable()
end

-- Use system popup theme.
local custom_popup_menu = cmp.config.window.bordered({
  winhighlight = 'Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel',
})

cmp.setup({
  window = {
    completion = custom_popup_menu,
    documentation = custom_popup_menu,
  },

  mapping = cmp.mapping.preset.insert({
    ['<tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif has_words_before() then
        cmp.complete()
      else
        fallback()
      end
    end, { 'i', 's' }),

    ['<s-tab>'] = cmp.mapping(function()
      if cmp.visible() then
        cmp.select_prev_item()
      end
    end, { 'i', 's' }),

    ['<enter>'] = cmp.mapping.confirm({ select = false }),
  }),

  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'path' },
    {
      name = 'buffer',
      option = { get_bufnrs = get_visible_buffers },
    },
  }),
})

-- Search completions
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    {
      name = 'buffer',
      option = { get_bufnrs = get_visible_buffers },
    },
  }),
  matching = { disallow_symbol_nonprefix_matching = false }
})

-- Ex mode completions. Breaks completion in VimL but improves Lua.
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' },
    { name = 'cmdline' },
  }),
})

-- Advertise extended completion capabilities.
core_lsp.on_start(function(client)
  client.capabilities = nvim_lsp.default_capabilities(client.capabilities)
end)
