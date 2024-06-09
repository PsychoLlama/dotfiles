local gitsigns = require('gitsigns')

gitsigns.setup({
  -- Default options.
})

vim.keymap.set('n', '<leader>hr', gitsigns.reset_hunk)
vim.keymap.set('n', '<leader>hs', gitsigns.stage_hunk)
vim.keymap.set('n', '<leader>hu', gitsigns.undo_stage_hunk)

vim.keymap.set('n', '<leader>hS', gitsigns.stage_buffer)
vim.keymap.set('n', '<leader>hR', gitsigns.reset_buffer)

vim.keymap.set('n', '<leader>hb', gitsigns.blame_line)
