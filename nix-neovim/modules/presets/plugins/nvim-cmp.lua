local cmp = require('cmp')

local function has_words_before()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

cmp.setup({
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
  }),

  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'path' },
    {
      name = 'buffer',
      option = {
        -- Pull completions from all visible buffers.
        get_bufnrs = function()
          return vim.iter(vim.api.nvim_tabpage_list_wins(0))
              :map(vim.api.nvim_win_get_buf)
              :totable()
        end,
      }
    },
  }),
})
