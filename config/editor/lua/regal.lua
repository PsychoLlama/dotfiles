local os = require('os')

local directory = vim.fn.getenv('HOME')

return {
  setup = function(opts)
    directory = vim.fn.expand(opts.directory)
  end,

  open_notes = function()
    vim.api.nvim_command("edit " .. directory)
  end,

  create_note = function()
    local sentence_title = vim.fn.input('Title: ')
    local normalized_title = vim.fn.tolower(vim.fn.tr(sentence_title, ' ', '-'))

    local timestamp = vim.fn.localtime()
    local iso_8601 = vim.fn.strftime('%Y-%m-%dT%H:%M:%SZ', timestamp)

    local filename = timestamp .. '-' .. normalized_title .. '.md'
    local filepath = directory .. '/' .. filename

    vim.fn.writefile(
      {
        '---',
        'title: ' .. sentence_title,
        'createdAt: ' .. iso_8601,
        '---',
        '',
        '',
      },
      filepath
    )

    vim.api.nvim_command('edit ' .. filepath)
    vim.fn.setpos('.', { vim.fn.bufnr('.'), vim.fn.line('$'), 1, 0 })
  end,
}
