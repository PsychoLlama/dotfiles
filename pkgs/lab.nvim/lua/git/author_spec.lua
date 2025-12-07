local stub = require('luassert.stub')

describe('git.author', function()
  local author

  before_each(function()
    -- Clear module cache to get fresh state
    package.loaded['git.author'] = nil
    package.loaded['git.blame'] = nil
  end)

  describe('module loading', function()
    it('loads without error', function()
      assert.has_no_error(function()
        author = require('git.author')
      end)
    end)

    it('exports command function', function()
      author = require('git.author')
      assert.is_function(author.command)
    end)
  end)

  describe('command', function()
    local snapshot

    before_each(function()
      snapshot = assert:snapshot()
      author = require('git.author')
    end)

    after_each(function()
      snapshot:revert()
    end)

    it('returns early if buffer is modified', function()
      vim.bo = { modified = true }
      local print_stub = stub(_G, 'print')

      author.command(1, 10)

      assert.stub(print_stub).was.called_with('Save your changes first.')
    end)

    it('returns early for directories', function()
      vim.bo = { modified = false }
      ---@diagnostic disable-next-line: undefined-field
      stub(vim.fn, 'expand').returns('/some/directory')
      ---@diagnostic disable-next-line: undefined-field
      stub(vim.fn, 'isdirectory').returns(1)
      local print_stub = stub(_G, 'print')

      author.command(1, 10)

      assert.stub(print_stub).was.called_with('Uh, this is a directory')
    end)
  end)
end)
