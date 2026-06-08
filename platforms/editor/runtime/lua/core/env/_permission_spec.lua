local stub = require('luassert.stub')

local memory = require('core.env._memory')
local permission = require('core.env._permission')

describe('core.env._permission', function()
  local snapshot

  before_each(function()
    snapshot = assert:snapshot()
  end)

  after_each(function()
    snapshot:revert()
  end)

  describe('ask', function()
    it('maps the first choice to "allow"', function()
      ---@diagnostic disable-next-line: undefined-field
      stub(vim.fn, 'inputlist').returns(1)

      assert.are.equal('allow', permission.ask('/foo', {}))
    end)

    it('maps the second choice to "deny"', function()
      ---@diagnostic disable-next-line: undefined-field
      stub(vim.fn, 'inputlist').returns(2)

      assert.are.equal('deny', permission.ask('/foo', {}))
    end)

    it('treats any other choice as "unknown"', function()
      ---@diagnostic disable-next-line: undefined-field
      stub(vim.fn, 'inputlist').returns(0)

      assert.are.equal('unknown', permission.ask('/foo', {}))
    end)

    it('does not require opts', function()
      ---@diagnostic disable-next-line: undefined-field
      stub(vim.fn, 'inputlist').returns(1)

      assert.has_no_error(function()
        permission.ask('/foo')
      end)
    end)
  end)

  describe('check_memory_or_ask', function()
    it('honours a remembered "allow" without asking', function()
      stub(memory, 'get_permission').returns('allow')
      local ask = stub(permission, 'ask')
      local update = stub(memory, 'update_permission')

      assert.are.equal(
        'allow',
        permission.check_memory_or_ask('/foo/vimrc', {})
      )
      assert.stub(ask).was.not_called()
      assert.stub(update).was.not_called()
    end)

    it('honours a remembered "deny" without asking', function()
      stub(memory, 'get_permission').returns('deny')
      local ask = stub(permission, 'ask')

      assert.are.equal(
        'deny',
        permission.check_memory_or_ask('/foo/vimrc', {})
      )
      assert.stub(ask).was.not_called()
    end)

    it('resolves permission against the containing directory', function()
      local get = stub(memory, 'get_permission').returns('allow')

      permission.check_memory_or_ask('/foo/vimrc', {})

      assert.stub(get).was.called_with('/foo')
    end)

    it('asks and remembers the answer against the directory', function()
      stub(memory, 'get_permission').returns('unknown')
      stub(permission, 'ask').returns('allow')
      local update = stub(memory, 'update_permission')

      assert.are.equal(
        'allow',
        permission.check_memory_or_ask('/foo/vimrc', {})
      )
      assert.stub(update).was.called_with('/foo', 'allow')
    end)
  end)
end)
