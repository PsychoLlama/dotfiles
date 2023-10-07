local config = require('regal.config')
local utils = require('regal.utils')

local function open()
  vim.cmd.edit(vim.fn.fnameescape(config.slip_box))

  -- Natural sort puts the newest notes at the obttom.
  utils.go_to_last_line()
end

local function create()
  -- Start with an open notebook.
  open()

  -- Deferred one tick to allow the notebook to render.
  vim.schedule(function()
    vim.ui.input({ prompt = 'Title: ' }, function(title)
      if title == nil or vim.trim(title) == '' then
        return
      end

      local timestamp = vim.fn.localtime()
      local iso_8601 = vim.fn.strftime('%Y-%m-%dT%H:%M:%SZ', timestamp)

      local filename = timestamp .. '-' .. utils.normalize_title(title) .. '.md'
      local filepath = config.slip_box .. '/' .. filename

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

      vim.cmd.edit(filepath)
      utils.go_to_last_line()

      vim.api.nvim_feedkeys('I', 'n', true)
    end)
  end)
end

return {
  open = open,
  create = create,
}
