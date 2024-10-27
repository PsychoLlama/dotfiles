-- Very experimental. I'm not doing much with Neotest yet.

return function()
  require('neotest').setup({
    adapters = {
      require('neotest-vitest'),
    },
  })
end
