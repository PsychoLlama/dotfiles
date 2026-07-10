local M = {}

--- A parsed frontmatter block. Keyed by field name; values are the raw text
--- after the colon. Only the flat `key: value` syntax we author is modeled.
--- @alias note.Frontmatter table<string, string>

--- Fields emitted first, in this order, when rendering frontmatter back out.
--- Any other fields follow in sorted order so nothing is silently dropped.
local KEY_ORDER = { 'title', 'createdAt' }

--- Parse the frontmatter block at the top of a file's lines.
--- @param lines string[] The file's lines.
--- @return note.Frontmatter fields Parsed pairs (empty if there is no block).
--- @return integer size How many lines the block spans, both `---` fences
---   included. Zero when there is no valid block.
function M.parse(lines)
  if lines[1] ~= '---' then
    return {}, 0
  end

  local fields = {}
  for i = 2, #lines do
    local line = lines[i]
    if line == '---' then
      return fields, i
    end

    local key, value = line:match('^(%w[%w_]*):%s*(.*)$')
    if key then
      fields[key] = value
    end
  end

  -- No closing fence: treat the whole thing as bodytext, not frontmatter.
  return {}, 0
end

--- Render frontmatter fields into fenced lines.
--- @param fields note.Frontmatter
--- @return string[] lines Including the surrounding `---` fences.
function M.generate(fields)
  local lines = { '---' }

  local emitted = {}
  for _, key in ipairs(KEY_ORDER) do
    if fields[key] ~= nil then
      table.insert(lines, key .. ': ' .. fields[key])
      emitted[key] = true
    end
  end

  -- Preserve any unrecognized keys in a stable (sorted) order.
  local rest = {}
  for key in pairs(fields) do
    if not emitted[key] then
      table.insert(rest, key)
    end
  end
  table.sort(rest)
  for _, key in ipairs(rest) do
    table.insert(lines, key .. ': ' .. fields[key])
  end

  table.insert(lines, '---')
  return lines
end

return M
