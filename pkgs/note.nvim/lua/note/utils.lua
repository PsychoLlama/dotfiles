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

return M
