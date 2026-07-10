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

--- Carry out a rename of the current buffer's note to `new_title`: rewrite the
--- frontmatter title, move the file, and leave no stale buffer behind.
--- @param old_path string Absolute path of the note being renamed.
--- @param new_title string
local function rename_to(old_path, new_title)
  local dir = vim.fs.dirname(old_path)
  local new_basename =
    utils.rename_filename(vim.fs.basename(old_path), new_title)
  local new_path = vim.fs.joinpath(dir, new_basename)

  -- Never clobber a different note that already claims the target name.
  if new_path ~= old_path and vim.uv.fs_stat(new_path) then
    vim.notify(
      'note.nvim: a note named ' .. new_basename .. ' already exists',
      vim.log.levels.WARN
    )
    return
  end

  -- Rewrite the title in the frontmatter block in place.
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local fields, size = frontmatter.parse(lines)
  fields.title = new_title
  vim.api.nvim_buf_set_lines(0, 0, size, false, frontmatter.generate(fields))

  if new_path == old_path then
    -- Only the title text changed (e.g. casing); just save.
    vim.cmd.write()
    return
  end

  -- Point the buffer at the new name, persist it, then remove the old file
  -- and any buffer neovim kept around for the old name.
  vim.api.nvim_buf_set_name(0, new_path)
  vim.cmd.write({ bang = true })
  os.remove(old_path)

  local current = vim.api.nvim_get_current_buf()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if buf ~= current and vim.api.nvim_buf_get_name(buf) == old_path then
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end
end

--- Rename the note in the current buffer. Prompts for a new title (prefilled
--- with the current one) when none is given. Refuses to run over unsaved
--- changes so nothing is lost.
--- @param new_title? string
function M.rename(new_title)
  if not config.get() then
    return
  end

  if vim.bo.modified then
    vim.notify('note.nvim: save your changes first', vim.log.levels.WARN)
    return
  end

  local old_path = vim.api.nvim_buf_get_name(0)
  if old_path == '' then
    vim.notify('note.nvim: no file to rename', vim.log.levels.WARN)
    return
  end

  if new_title ~= nil then
    if vim.trim(new_title) ~= '' then
      rename_to(old_path, new_title)
    end
    return
  end

  local fields =
    frontmatter.parse(vim.api.nvim_buf_get_lines(0, 0, -1, false))
  vim.ui.input(
    { prompt = 'Note Title: ', default = fields.title or '' },
    function(input)
      if input == nil or vim.trim(input) == '' then
        return
      end

      rename_to(old_path, input)
    end
  )
end

return M
