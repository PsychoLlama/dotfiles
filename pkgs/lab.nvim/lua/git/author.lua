local blame = require('git.blame')

local M = {}

---Calculate ownership percentages for authors
---@param authors table<string, integer>
---@param total number
---@return {name: string, percentage: integer}[]
local function get_author_ownership(authors, total)
  local result = {}

  for name, lines in pairs(authors) do
    local percentage = math.floor((lines / total) * 100 + 0.5)
    table.insert(result, { name = name, percentage = percentage })
  end

  table.sort(result, function(a, b)
    return a.percentage > b.percentage
  end)

  return result
end

---Find authors for a range of lines
---@param start_line integer
---@param end_line integer
---@return {name: string, percentage: integer}[]|nil
local function find_authors_for_range(start_line, end_line)
  local file = vim.fn.resolve(vim.fn.expand('%:p'))
  local blames = blame.get({
    file = file,
    ranges = { { start_line, end_line } },
  })

  if not blames then
    return nil
  end

  local author_counts = {}
  for _, b in ipairs(blames) do
    local name = b.author.name or '<unknown>'
    author_counts[name] = (author_counts[name] or 0) + 1
  end

  local total = end_line - start_line + 1
  return get_author_ownership(author_counts, total)
end

---Print detailed info for a single line
---@param line integer
local function print_line_details(line)
  local file = vim.fn.expand('%:p')
  local blames = blame.get({
    file = file,
    ranges = { { line, line } },
  })

  if not blames or #blames == 0 then
    return
  end

  local details = blames[1]
  local date = os.date('%m/%d/%Y', details.author.time)

  vim.api.nvim_echo({
    { details.sha:sub(1, 7), 'String' },
    { ': ' .. details.author.name .. ' (' },
    { date, 'Type' },
    { ')' },
  }, false, {})

  print(details.summary)
end

---Main command handler for :Author
---@param start_line integer
---@param end_line integer
function M.command(start_line, end_line)
  if vim.bo.modified then
    print('Save your changes first.')
    return
  end

  local path = vim.fn.expand('%:p')
  if vim.fn.isdirectory(path) == 1 then
    print('Uh, this is a directory')
    return
  end

  -- Single line: show detailed info
  if start_line == end_line then
    return print_line_details(start_line)
  end

  -- Range: show ownership breakdown
  local authors = find_authors_for_range(start_line, end_line)
  if not authors then
    return
  end

  for i, author in ipairs(authors) do
    vim.api.nvim_echo({
      { author.name },
      { ' (' .. author.percentage .. '%)', 'Comment' },
      { i < #authors and '\n' or '' },
    }, false, {})
  end
end

return M
