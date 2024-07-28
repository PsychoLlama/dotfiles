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
