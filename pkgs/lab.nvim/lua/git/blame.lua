local M = {}

---@class GitAuthor
---@field name string|nil
---@field email string|nil
---@field time integer|nil
---@field zone string|nil

---@class GitLine
---@field prev_number integer|nil
---@field contents string|nil
---@field number integer|nil

---@class GitBlame
---@field committer GitAuthor
---@field author GitAuthor
---@field prev_filename string|nil
---@field line GitLine
---@field prev_sha string|nil
---@field summary string|nil
---@field sha string|nil

---@class BlameConfig
---@field file string
---@field ranges? integer[][] Line ranges like {{1, 2}, {4, 5}}
---@field revision? string Git revision to blame at

local AUTHOR_HEADER_MAP = {
  [''] = 'name',
  ['-mail'] = 'email',
  ['-time'] = 'time',
  ['-tz'] = 'zone',
}

local function create_author()
  return { name = nil, email = nil, time = nil, zone = nil }
end

local function create_blame()
  return {
    committer = create_author(),
    author = create_author(),
    prev_filename = nil,
    line = { prev_number = nil, contents = nil, number = nil },
    prev_sha = nil,
    summary = nil,
    sha = nil,
  }
end

local function is_sha(header)
  return #header == 40 and header:match('^[0-9a-f]+$') ~= nil
end

local function get_header(line)
  local space_idx = line:find(' ')
  if space_idx then
    return line:sub(1, space_idx - 1)
  end
  return line
end

local function strip_header(header, line)
  return line:sub(#header + 2)
end

local function add_sha_details(line, blame)
  local parts = vim.split(line, ' ')
  blame.sha = parts[1]
  blame.line.prev_number = tonumber(parts[2])
  blame.line.number = tonumber(parts[3])
end

local function add_line_details(line, blame)
  blame.line.contents = line:sub(2) -- Strip leading tab
end

local function add_author_details(header, line, author, my_name)
  local header_key = header:gsub('^author', ''):gsub('^committer', '')
  local key = AUTHOR_HEADER_MAP[header_key]
  local content = strip_header(header, line)

  if key == 'email' then
    content = content:sub(2, -2) -- Trim angle brackets
  elseif key == 'time' then
    content = tonumber(content)
  elseif key == 'name' and content:lower():match('not committed yet') then
    content = my_name
  end

  author[key] = content
end

local function add_prev_sha_details(line, blame)
  local value = strip_header('previous', line)
  local space_idx = value:find(' ')
  blame.prev_sha = value:sub(1, space_idx - 1)
  blame.prev_filename = value:sub(space_idx + 1)
end

local function add_summary_details(line, blame)
  blame.summary = strip_header('summary', line)
end

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

---Parse git blame porcelain output into structured data
---@param output string[]
---@param my_name string
---@return GitBlame[]
function M.parse(output, my_name)
  local blames = {}
  ---@type GitBlame|nil
  local blame = nil

  for _, line in ipairs(output) do
    local header = get_header(line)

    if is_sha(header) then
      blame = create_blame()
      add_sha_details(line, blame)
      table.insert(blames, blame)
    elseif blame and line:sub(1, 1) == '\t' then
      add_line_details(line, blame)
    elseif blame and header:match('^author') then
      add_author_details(header, line, blame.author, my_name)
    elseif blame and header:match('^committer') then
      add_author_details(header, line, blame.committer, my_name)
    elseif blame and header == 'previous' then
      add_prev_sha_details(line, blame)
    elseif blame and header == 'summary' then
      add_summary_details(line, blame)
    end
  end

  return blames
end

---Check if a file is tracked in git
---@param file string
---@param revision? string
---@return boolean
function M.is_tracked(file, revision)
  local repo_root = M.find_repo_root(file)
  if not repo_root then
    return false
  end

  local cmd = { 'git', 'ls-files', '--error-unmatch' }
  if revision then
    table.insert(cmd, '--with-tree=' .. revision)
  end
  table.insert(cmd, '--')
  table.insert(cmd, file)

  local result = vim.system(cmd, { cwd = repo_root, text = true }):wait()
  return result.code == 0
end

---Find the git repo root for a file
---@param path string
---@return string|nil
function M.find_repo_root(path)
  local dir = vim.fn.isdirectory(path) == 1 and path
    or vim.fn.fnamemodify(path, ':h')
  local result = vim
    .system({ 'git', 'rev-parse', '--show-toplevel' }, { cwd = dir, text = true })
    :wait()

  if result.code ~= 0 then
    return nil
  end

  return vim.trim(result.stdout)
end

---Get blame for a file
---@param config BlameConfig
---@return GitBlame[]|nil
function M.get(config)
  if not M.is_tracked(config.file, config.revision) then
    vim.api.nvim_echo({
      { 'Error:', 'Error' },
      { " Can't git-blame an untracked file." },
    }, true, {})
    return nil
  end

  local cmd = { 'git', 'blame', '--line-porcelain' }

  if config.ranges then
    for _, range in ipairs(config.ranges) do
      table.insert(cmd, '-L')
      table.insert(cmd, range[1] .. ',' .. range[2])
    end
  end

  if config.revision then
    table.insert(cmd, config.revision)
  end

  table.insert(cmd, '--')
  table.insert(cmd, config.file)

  local dir = vim.fn.fnamemodify(config.file, ':h')
  local result = vim.system(cmd, { cwd = dir, text = true }):wait()

  if result.code ~= 0 then
    return nil
  end

  local output = vim.split(result.stdout, '\n', { trimempty = true })
  local my_name = get_user_name(config.file)

  return M.parse(output, my_name)
end

return M
