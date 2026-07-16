local M = {}

---@class GitLineDetails
---@field sha string
---@field name string
---@field time integer
---@field summary string

-- `git blame` has no custom output format, but `--line-porcelain` emits one
-- self-contained block per source line with every field on its own
-- `key value` line. We only need four of them, so rather than parse the whole
-- block we prefix-match the lines we care about. The `author ` header carries a
-- trailing space, so it never collides with `author-mail`/`-time`/`-tz`.

---Count how many lines each author owns in line-porcelain output.
---Every source line emits exactly one `author ` header, so counting those
---headers counts lines.
---@param output string[]
---@param my_name string Name to attribute uncommitted lines to
---@return table<string, integer>
function M.count_authors(output, my_name)
  local counts = {}

  for _, line in ipairs(output) do
    local name = line:match('^author (.+)$')
    if name then
      if name == 'Not Committed Yet' then
        name = my_name
      end
      counts[name] = (counts[name] or 0) + 1
    end
  end

  return counts
end

---Extract sha, author, time, and summary from a single line's blame block.
---@param output string[]
---@param my_name string Name to attribute uncommitted lines to
---@return GitLineDetails|nil
function M.line_details(output, my_name)
  local sha = output[1] and output[1]:match('^(%x+) %d+ %d+')
  if not sha then
    return nil
  end

  local details = { sha = sha }
  for _, line in ipairs(output) do
    details.name = details.name or line:match('^author (.+)$')
    details.time = details.time
      or tonumber(line:match('^author%-time (%d+)$'))
    details.summary = details.summary or line:match('^summary (.+)$')
  end

  if details.name == 'Not Committed Yet' then
    details.name = my_name
  end

  return details
end

---Find the git repo root for a file
---@param path string
---@return string|nil
local function find_repo_root(path)
  local dir = vim.fn.isdirectory(path) == 1 and path
    or vim.fn.fnamemodify(path, ':h')
  return vim.fs.root(dir, '.git')
end

---Check if a file is tracked in git
---@param file string
---@return boolean
local function is_tracked(file)
  local repo_root = find_repo_root(file)
  if not repo_root then
    return false
  end

  local result = vim
    .system(
      { 'git', 'ls-files', '--error-unmatch', '--', file },
      { cwd = repo_root, text = true }
    )
    :wait()

  return result.code == 0
end

---Resolve the name to credit for uncommitted lines.
---@param file string
---@return string
local function get_user_name(file)
  local dir = vim.fn.fnamemodify(file, ':h')
  local result = vim
    .system({ 'git', 'config', 'user.name' }, { cwd = dir, text = true })
    :wait()

  if result.code ~= 0 or not result.stdout or result.stdout == '' then
    return '<current user>'
  end

  return vim.trim(result.stdout)
end

---Run `git blame --line-porcelain` over a single line range.
---@param file string
---@param start_line integer
---@param end_line integer
---@return string[]|nil
local function blame_range(file, start_line, end_line)
  if not is_tracked(file) then
    vim.api.nvim_echo({
      { 'Error:', 'Error' },
      { " Can't git-blame an untracked file." },
    }, true, {})
    return nil
  end

  local cmd = {
    'git',
    'blame',
    '--line-porcelain',
    '-L',
    start_line .. ',' .. end_line,
    '--',
    file,
  }

  local result = vim
    .system(cmd, { cwd = vim.fn.fnamemodify(file, ':h'), text = true })
    :wait()

  if result.code ~= 0 then
    return nil
  end

  return vim.split(result.stdout, '\n', { trimempty = true })
end

---Authorship line counts for a range.
---@param file string
---@param start_line integer
---@param end_line integer
---@return table<string, integer>|nil
function M.authors_in_range(file, start_line, end_line)
  local output = blame_range(file, start_line, end_line)
  if not output then
    return nil
  end

  return M.count_authors(output, get_user_name(file))
end

---Blame details for a single line.
---@param file string
---@param line integer
---@return GitLineDetails|nil
function M.line(file, line)
  local output = blame_range(file, line, line)
  if not output then
    return nil
  end

  return M.line_details(output, get_user_name(file))
end

return M
