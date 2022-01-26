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
    local function normalize_title(title)
      return vim.fn.tolower(vim.fn.tr(title, ' ', '-'))
    end

    vim.ui.input({ prompt = 'Title: ' }, function(title)
      if title == nil or vim.trim(title) == '' then
        return
      end

      local timestamp = vim.fn.localtime()
      local iso_8601 = vim.fn.strftime('%Y-%m-%dT%H:%M:%SZ', timestamp)

      local filename = timestamp .. '-' .. normalize_title(title) .. '.md'
      local filepath = directory .. '/' .. filename

      vim.fn.writefile(
        {
          '---',
          'title: ' .. title,
          'createdAt: ' .. iso_8601,
          '---',
          '',
          '',
        },
        filepath
      )

      vim.api.nvim_command('edit ' .. filepath)
      vim.api.nvim_feedkeys('GI', 'n', true)
    end)
  end,
}
