return function(conventions)
  vim.api.nvim_set_keymap('n', '<leader>a', '<plug>(alternaut-toggle)', {
    desc = 'Switch between related files',
  })

  -- TODO:
  -- 1. Rewrite Alternaut in Lua
  -- 2. Pass a list of rules instead of filetype mappings
  -- 3. Match rules using functions, shorthand for filetypes
  -- 4. Partition by modes (header files, styles, tests)
  vim.g['alternaut#conventions'] = conventions
end
