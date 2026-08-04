return function(config)
  local alternaut = require('alternaut')

  vim.keymap.set('n', '<A-0>', function()
    alternaut.toggle('style')
  end)

  vim.keymap.set('n', '<A-9>', function()
    alternaut.toggle('template')
  end)

  vim.keymap.set('n', '<A-8>', function()
    alternaut.toggle('header')
  end)

  vim.keymap.set('n', '<A-\\>', function()
    alternaut.toggle('test')
  end)

  alternaut.setup(config)
end
