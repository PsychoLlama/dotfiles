local markdown = require('markdown')

markdown.setup({
  on_attach = function(bufnr)
    local opts = { buffer = bufnr }

    -- Toggle task checkboxes
    vim.keymap.set('n', '<c-x>', '<cmd>MDTaskToggle<cr>', opts)
    vim.keymap.set('x', '<c-x>', ':MDTaskToggle<cr>', opts)

    -- Insert list items
    vim.keymap.set({ 'n', 'i' }, '<m-o>', '<cmd>MDListItemBelow<cr>', opts)
    vim.keymap.set({ 'n', 'i' }, '<m-O>', '<cmd>MDListItemAbove<cr>', opts)
  end,
})
