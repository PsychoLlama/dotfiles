return function(opts)
  vim.api.nvim_set_keymap('n', '<leader>m', '<cmd>MarkdownPreview<cr>', {
    noremap = true,
    silent = true,
    desc = 'Preview Markdown',
  })

  vim.g.mkdp_browser = opts.browser
end
