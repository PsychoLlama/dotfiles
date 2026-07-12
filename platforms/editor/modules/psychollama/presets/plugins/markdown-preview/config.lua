return function()
  vim.api.nvim_set_keymap('n', '<leader>mp', '<cmd>MarkdownPreview<cr>', {
    noremap = true,
    silent = true,
    desc = 'Preview Markdown',
  })
end
