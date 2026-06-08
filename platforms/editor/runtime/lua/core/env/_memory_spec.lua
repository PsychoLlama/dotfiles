local stub = require('luassert.stub')

local memory = require('core.env._memory')

--- Build an in-memory stand-in for a file handle so tests never touch disk.
--- @param content string Bytes returned by `read`.
local function fake_file(content)
  local writes = {}
  return {
    read = function()
      return content
    end,
    write = function(_, data)
      table.insert(writes, data)
      return true
    end,
    close = function()
      return true
    end,
    writes = writes,
  }
end

describe('core.env._memory', function()
  local snapshot

  before_each(function()
    snapshot = assert:snapshot()
  end)

  after_each(function()
    snapshot:revert()
  end)

  describe('get_file', function()
    it('lives under the state directory', function()
      ---@diagnostic disable-next-line: undefined-field
      stub(vim.fn, 'stdpath').returns('/state')

      assert.are.equal('/state/dynamic-sources.json', memory.get_file())
    end)
  end)

  describe('load', function()
    it('returns an empty table when the file is missing', function()
      stub(io, 'open').returns(nil)

      assert.are.same({}, memory.load())
    end)

    it('decodes the stored JSON', function()
      local handle = fake_file('{"/foo":"allow","/bar":"deny"}')
      stub(io, 'open').returns(handle)

      assert.are.same(
        { ['/foo'] = 'allow', ['/bar'] = 'deny' },
        memory.load()
      )
    end)
  end)

  describe('save', function()
    it('writes the settings as JSON', function()
      local handle = fake_file('')
      stub(io, 'open').returns(handle)

      memory.save({ ['/foo'] = 'allow' })

      local written = table.concat(handle.writes)
      assert.are.same({ ['/foo'] = 'allow' }, vim.json.decode(written))
    end)

    it('warns instead of throwing when the file cannot be opened', function()
      stub(io, 'open').returns(nil)
      local notify = stub(vim, 'notify')

      assert.has_no_error(function()
        memory.save({ ['/foo'] = 'allow' })
      end)
      assert.stub(notify).was.called()
    end)
  end)

  describe('update_permission', function()
    it('merges the new permission into existing memory', function()
      stub(memory, 'load').returns({ ['/foo'] = 'allow' })
      local save = stub(memory, 'save')

      memory.update_permission('/bar', 'deny')

      assert
        .stub(save).was
        .called_with({ ['/foo'] = 'allow', ['/bar'] = 'deny' })
    end)
  end)

  describe('get_permission', function()
    it('returns the remembered permission', function()
      stub(memory, 'load').returns({ ['/foo'] = 'allow' })

      assert.are.equal('allow', memory.get_permission('/foo'))
    end)

    it('returns "unknown" for files we have not seen', function()
      stub(memory, 'load').returns({})

      assert.are.equal('unknown', memory.get_permission('/foo'))
    end)
  end)
end)
