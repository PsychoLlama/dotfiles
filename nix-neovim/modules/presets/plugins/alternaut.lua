vim.api.nvim_set_keymap('n', '<leader>a', '<plug>(alternaut-toggle)', {
  desc = 'Switch between related files',
})

local typescript = {
  directory_naming_conventions = { "__tests__" },
  file_extensions = { "ts", "tsx", "js", "jsx" },
  file_naming_conventions = {
    "{name}.test.{ext}",
    "{name}.unit.{ext}"
  }
}

local javascript = {
  directory_naming_conventions = { "__tests__" },
  file_extensions = { "js" },
  file_naming_conventions = { "{name}.test.{ext}" }
}

local vader = {
  directory_naming_conventions = { "tests" },
  file_extensions = { "vim", "vader" },
  file_naming_conventions = { "{name}.{ext}" }
}

-- TODO:
-- 1. Rewrite Alternaut in Lua
-- 2. Pass a list of rules instead of filetype mappings
-- 3. Match rules using functions, shorthand for filetypes
-- 4. Partition by modes (header files, styles, tests)
vim.g['alternaut#conventions'] = {
  ["typescript"] = typescript,
  ["typescript.tsx"] = typescript,
  ["typescriptreact"] = typescript,

  ["javascript"] = javascript,
  ["javascript.jsx"] = javascript,

  ["vader"] = vader,
  ["vim"] = vader,

  ["python"] = {
    directory_naming_conventions = { "tests" },
    file_extensions = { "py" },
    file_naming_conventions = {
      "test_{name}.{ext}",
      "{name}.{ext}"
    }
  },
}
