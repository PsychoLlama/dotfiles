local M = {}

--- Move the cursor to the last line of the current buffer.
function M.go_to_last_line()
  vim.fn.cursor(vim.fn.line('$'), 1)
end

--- Slugify a note title for use in a filename: lowercased, with spaces and
--- colons collapsed to hyphens.
--- @param title string
--- @return string
function M.normalize_title(title)
  return vim.fn.tolower(vim.fn.tr(title, ' :', '--'))
end

--- Swap the slug of a note filename for one derived from `new_title`, keeping
--- the leading `<timestamp>-` prefix and the extension so the note's identity
--- and sort order are preserved.
--- @param basename string e.g. "1699900000-old-title.md"
--- @param new_title string
--- @return string
function M.rename_filename(basename, new_title)
  local prefix = basename:match('^(%d+%-)') or ''
  local ext = basename:match('(%.[^.]+)$') or '.md'
  return prefix .. M.normalize_title(new_title) .. ext
end

return M
