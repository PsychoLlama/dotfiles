return function()
  require('copilot').setup({
    suggestion = {
      enabled = true,
      auto_trigger = true,
      keymap = {
        accept = '<C-j>',
      },
    },
  })
end
