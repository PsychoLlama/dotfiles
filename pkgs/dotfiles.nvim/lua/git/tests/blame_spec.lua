local blame = require('git.blame')

-- Blocks as `git blame --line-porcelain` emits them: a sha header, then
-- `key value` lines, then the tab-prefixed source line.
local function block(sha, author, time, summary, contents)
  return {
    sha .. ' 1 1 1',
    'author ' .. author,
    'author-mail <' .. author .. '@example.com>',
    'author-time ' .. time,
    'author-tz -0800',
    'committer ' .. author,
    'committer-mail <' .. author .. '@example.com>',
    'committer-time ' .. time,
    'committer-tz -0800',
    'summary ' .. summary,
    'filename test.lua',
    '\t' .. contents,
  }
end

local function concat(...)
  local out = {}
  for _, block_lines in ipairs({ ... }) do
    for _, line in ipairs(block_lines) do
      table.insert(out, line)
    end
  end
  return out
end

describe('git.blame', function()
  describe('count_authors', function()
    it('counts lines per author', function()
      local output = concat(
        block('aaaa', 'Alice', '1699900000', 'First', 'line one'),
        block('bbbb', 'Bob', '1699900100', 'Second', 'line two'),
        block('cccc', 'Alice', '1699900200', 'Third', 'line three')
      )

      local counts = blame.count_authors(output, 'Test User')

      assert.are.equal(2, counts['Alice'])
      assert.are.equal(1, counts['Bob'])
    end)

    it('attributes uncommitted lines to the given name', function()
      local output = block(
        '0000000000000000000000000000000000000000',
        'Not Committed Yet',
        '1699900000',
        'Uncommitted',
        'local x = 1'
      )

      local counts = blame.count_authors(output, 'My Name')

      assert.are.equal(1, counts['My Name'])
      assert.is_nil(counts['Not Committed Yet'])
    end)

    it(
      'does not mistake author-mail/-time/-tz for the author header',
      function()
        local output =
          block('aaaa', 'Alice', '1699900000', 'First', 'line one')

        local counts = blame.count_authors(output, 'Test User')

        -- One author header per line, nothing leaked from the sibling headers.
        assert.are.equal(1, counts['Alice'])
        assert.are.same({ Alice = 1 }, counts)
      end
    )
  end)

  describe('line_details', function()
    it('extracts sha, author, time, and summary', function()
      local output = block(
        'abcd1234567890abcd1234567890abcd12345678',
        'John Doe',
        '1699900000',
        'Initial commit',
        'local x = 1'
      )

      local details = assert(blame.line_details(output, 'Test User'))

      assert.are.equal(
        'abcd1234567890abcd1234567890abcd12345678',
        details.sha
      )
      assert.are.equal('John Doe', details.name)
      assert.are.equal(1699900000, details.time)
      assert.are.equal('Initial commit', details.summary)
    end)

    it('attributes uncommitted lines to the given name', function()
      local output = block(
        '0000000000000000000000000000000000000000',
        'Not Committed Yet',
        '1699900000',
        'Uncommitted',
        'local x = 1'
      )

      local details = assert(blame.line_details(output, 'My Name'))

      assert.are.equal('My Name', details.name)
    end)

    it('returns nil without a sha header', function()
      assert.is_nil(blame.line_details({}, 'Test User'))
      assert.is_nil(blame.line_details({ 'author John Doe' }, 'Test User'))
    end)
  end)
end)
