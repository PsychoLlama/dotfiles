local config = require('note.config')
local frontmatter = require('note.frontmatter')
local utils = require('note.utils')

local M = {}

--- Open the slip box notebook with the cursor on the newest note.
function M.open()
  local conf = config.get()
  if not conf then
    return
  end

  vim.cmd.edit(vim.fn.fnameescape(conf.path))

  -- Natural sort puts the newest notes at the bottom.
  utils.go_to_last_line()
end

--- Prompt for a title, then create and open a new note in the slip box.
function M.create()
  local conf = config.get()
  if not conf then
    return
  end

  -- Start with an open notebook.
  M.open()

  -- Deferred one tick to allow the notebook to render.
  vim.schedule(function()
    vim.ui.input({ prompt = 'Note Title: ' }, function(title)
      if title == nil or vim.trim(title) == '' then
        return
      end

      local timestamp = vim.fn.localtime()
      local iso_8601 = vim.fn.strftime('%Y-%m-%dT%H:%M:%SZ', timestamp)

      local filename = timestamp
        .. '-'
        .. utils.normalize_title(title)
        .. '.md'
      local filepath = vim.fs.joinpath(conf.path, filename)

      local contents = frontmatter.generate({
        title = title,
        createdAt = iso_8601,
      })
      vim.list_extend(contents, { '', '' })
      vim.fn.writefile(contents, filepath)

      vim.cmd.edit(filepath)
      utils.go_to_last_line()

      vim.api.nvim_feedkeys('I', 'n', true)
    end)
  end)
end

return M
