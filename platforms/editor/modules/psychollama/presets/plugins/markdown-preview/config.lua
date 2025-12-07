return function()
  vim.api.nvim_set_keymap('n', '<leader>m', '<cmd>MarkdownPreview<cr>', {
    noremap = true,
    silent = true,
    desc = 'Preview Markdown',
  })
end
