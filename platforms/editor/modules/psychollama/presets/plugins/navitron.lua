local navitron = require('navitron')
local actions = require('navitron.actions')
local snacks = require('snacks')

navitron.setup({
  actions = {
    [actions.find_file] = function()
      snacks.picker.files({
        hidden = true,
        dirs = { vim.b.navitron.path },
      })
    end,

    [actions.find_directory] = function()
      -- TODO: Find a way to exclude files OR write a custom picker.
      snacks.picker.files({
        hidden = true,
        args = { '--type', 'd' },
        dirs = { vim.b.navitron.path },
      })
    end,
  },
})

vim.keymap.set('n', '<leader>[', function()
  if vim.tbl_contains({ 'navitron', 'netrw' }, vim.o.filetype) then
    return
  end

  local current_file = vim.fn.expand('%:p')
  local parent_directory = vim.fn.fnamemodify(current_file, ':p:h')
  navitron.open(parent_directory)
end)
