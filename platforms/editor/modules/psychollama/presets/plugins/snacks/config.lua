return function(config)
  local snacks = require('snacks')
  snacks.setup(config)

  if vim.tbl_get(config, 'picker', 'enabled') then
    vim.keymap.set('n', '<c-space>', snacks.picker.pickers)
    vim.keymap.set('n', '<leader>b', snacks.picker.buffers)
    vim.keymap.set('n', '<leader>f', snacks.picker.files)
  end
end
