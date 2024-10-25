vim.api.nvim_set_keymap('n', '<leader>m', '<cmd>MarkdownPreview<cr>', {
  noremap = true,
  silent = true,
  desc = 'Preview Markdown',
})

-- TODO: Pass this through nix using `dofile(...)` and a function.
vim.g.mkdp_preview = vim.g.markdown_preview or 'firefox'
