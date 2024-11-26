local gitsigns = require('gitsigns')

gitsigns.setup({
  -- Default options.
})

vim.keymap.set('n', '<leader>ha', gitsigns.stage_hunk) -- "hunk add"
vim.keymap.set('n', '<leader>hr', gitsigns.undo_stage_hunk) -- "hunk remove"
vim.keymap.set('n', '<leader>hd', gitsigns.reset_hunk) -- "hunk delete"

vim.keymap.set('n', '<leader>hA', gitsigns.stage_buffer)
vim.keymap.set('n', '<leader>hD', gitsigns.reset_buffer)

vim.keymap.set('n', '<leader>hb', gitsigns.blame_line)
