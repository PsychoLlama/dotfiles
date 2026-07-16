local blame = require('git.blame')

local M = {}

---Rank authors by ownership percentage, highest first.
---@param counts table<string, integer>
---@param total number
---@return {name: string, percentage: integer}[]
local function rank_authors(counts, total)
  local result = {}

  for name, lines in pairs(counts) do
    local percentage = math.floor((lines / total) * 100 + 0.5)
    table.insert(result, { name = name, percentage = percentage })
  end

  table.sort(result, function(a, b)
    return a.percentage > b.percentage
  end)

  return result
end

---Print detailed info for a single line
---@param line integer
local function print_line_details(line)
  local file = vim.fn.expand('%:p')
  local details = blame.line(file, line)
  if not details then
    return
  end

  local date = os.date('%m/%d/%Y', details.time) --[[@as string]]

  vim.api.nvim_echo({
    { details.sha:sub(1, 7), 'String' },
    { ': ' .. details.name .. ' (' },
    { date, 'Type' },
    { ')' },
  }, false, {})

  print(details.summary)
end

---Show the ownership breakdown for a range of lines
---@param start_line integer
---@param end_line integer
local function print_range_ownership(start_line, end_line)
  local file = vim.fn.resolve(vim.fn.expand('%:p'))
  local counts = blame.authors_in_range(file, start_line, end_line)
  if not counts then
    return
  end

  local total = end_line - start_line + 1
  local authors = rank_authors(counts, total)

  for i, author in ipairs(authors) do
    vim.api.nvim_echo({
      { author.name },
      { ' (' .. author.percentage .. '%)', 'Comment' },
      { i < #authors and '\n' or '' },
    }, false, {})
  end
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

  if start_line == end_line then
    print_line_details(start_line)
  else
    print_range_ownership(start_line, end_line)
  end
end

return M
