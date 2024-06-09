require('nvim-treesitter.configs').setup({
  highlight = { enable = true },
  indent = { enable = true },
  textobjects = {
    swap = {
      enable = true,
      swap_next = {
        ['g>'] = '@parameter.inner',
      },
      swap_previous = {
        ['g<'] = '@parameter.inner',
      },
    },
  },
})

---@see https://github.com/nvim-treesitter/nvim-treesitter#adding-parsers
require('nvim-treesitter.parsers').get_parser_configs().nu = {
  -- Default settings.
}
