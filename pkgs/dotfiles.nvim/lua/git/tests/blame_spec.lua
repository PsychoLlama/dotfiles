local blame = require('git.blame')

describe('git.blame', function()
  describe('parse', function()
    it('parses a single line blame', function()
      local output = {
        'abcd1234567890abcd1234567890abcd12345678 1 1 1',
        'author John Doe',
        'author-mail <john@example.com>',
        'author-time 1699900000',
        'author-tz -0800',
        'committer Jane Smith',
        'committer-mail <jane@example.com>',
        'committer-time 1699900100',
        'committer-tz -0800',
        'summary Initial commit',
        'filename test.lua',
        '\tlocal x = 1',
      }

      local result = blame.parse(output, 'Test User')

      assert.are.equal(1, #result)
      assert.are.equal(
        'abcd1234567890abcd1234567890abcd12345678',
        result[1].sha
      )
      assert.are.equal('John Doe', result[1].author.name)
      assert.are.equal('john@example.com', result[1].author.email)
      assert.are.equal(1699900000, result[1].author.time)
      assert.are.equal('-0800', result[1].author.zone)
      assert.are.equal('Jane Smith', result[1].committer.name)
      assert.are.equal('jane@example.com', result[1].committer.email)
      assert.are.equal('Initial commit', result[1].summary)
      assert.are.equal(1, result[1].line.number)
      assert.are.equal(1, result[1].line.prev_number)
      assert.are.equal('local x = 1', result[1].line.contents)
    end)

    it('parses multiple lines', function()
      local output = {
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 1 1 1',
        'author Alice',
        'author-mail <alice@example.com>',
        'author-time 1699900000',
        'author-tz -0800',
        'committer Alice',
        'committer-mail <alice@example.com>',
        'committer-time 1699900000',
        'committer-tz -0800',
        'summary First line',
        'filename test.lua',
        '\tline one',
        'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 2 2 1',
        'author Bob',
        'author-mail <bob@example.com>',
        'author-time 1699900100',
        'author-tz -0800',
        'committer Bob',
        'committer-mail <bob@example.com>',
        'committer-time 1699900100',
        'committer-tz -0800',
        'summary Second line',
        'filename test.lua',
        '\tline two',
      }

      local result = blame.parse(output, 'Test User')

      assert.are.equal(2, #result)
      assert.are.equal('Alice', result[1].author.name)
      assert.are.equal('line one', result[1].line.contents)
      assert.are.equal(1, result[1].line.number)
      assert.are.equal('Bob', result[2].author.name)
      assert.are.equal('line two', result[2].line.contents)
      assert.are.equal(2, result[2].line.number)
    end)

    it('handles uncommitted changes with current user name', function()
      local output = {
        '0000000000000000000000000000000000000000 1 1 1',
        'author Not Committed Yet',
        'author-mail <not.committed.yet>',
        'author-time 1699900000',
        'author-tz -0800',
        'committer Not Committed Yet',
        'committer-mail <not.committed.yet>',
        'committer-time 1699900000',
        'committer-tz -0800',
        'summary Uncommitted changes',
        'filename test.lua',
        '\tlocal x = 1',
      }

      local result = blame.parse(output, 'My Name')

      assert.are.equal('My Name', result[1].author.name)
    end)

    it('parses previous commit info', function()
      local output = {
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 5 10 1',
        'author John',
        'author-mail <john@example.com>',
        'author-time 1699900000',
        'author-tz -0800',
        'committer John',
        'committer-mail <john@example.com>',
        'committer-time 1699900000',
        'committer-tz -0800',
        'summary Some commit',
        'previous bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb old_name.lua',
        'filename new_name.lua',
        '\tcontent',
      }

      local result = blame.parse(output, 'Test User')

      assert.are.equal(
        'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
        result[1].prev_sha
      )
      assert.are.equal('old_name.lua', result[1].prev_filename)
      assert.are.equal(5, result[1].line.prev_number)
      assert.are.equal(10, result[1].line.number)
    end)

    it('handles empty content lines', function()
      local output = {
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 1 1 1',
        'author John',
        'author-mail <john@example.com>',
        'author-time 1699900000',
        'author-tz -0800',
        'committer John',
        'committer-mail <john@example.com>',
        'committer-time 1699900000',
        'committer-tz -0800',
        'summary Some commit',
        'filename test.lua',
        '\t',
      }

      local result = blame.parse(output, 'Test User')

      assert.are.equal('', result[1].line.contents)
    end)
  end)
end)
