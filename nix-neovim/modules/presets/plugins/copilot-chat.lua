local copilot = require('CopilotChat')

copilot.setup({
  show_help = false,
  auto_insert_mode = true,
  question_header = '## Overlord ',
  answer_header = '## Computer ',
  mappings = {
    -- Don't break scroll mappings.
    accept_diff = {
      normal = '<leader>y',
      visual = '<leader>y',
    },
  },
})

vim.keymap.set('n', '<leader>c', function()
  copilot.toggle()
end)

vim.api.nvim_create_autocmd('BufEnter', {
  group = vim.api.nvim_create_augroup('copilot_settings', {}),
  pattern = 'copilot-*',
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.signcolumn = 'no'
  end,
})
