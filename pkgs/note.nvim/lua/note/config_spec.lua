local stub = require('luassert.stub')

describe('note.config', function()
  local config
  local snapshot

  before_each(function()
    -- Reset the module's private state between tests.
    package.loaded['note.config'] = nil
    config = require('note.config')
    snapshot = assert:snapshot()
  end)

  after_each(function()
    snapshot:revert()
  end)

  describe('set', function()
    it('warns and stores nothing when no options are given', function()
      local notify = stub(vim, 'notify')

      config.set(nil)

      assert.stub(notify).was.called_at_least(1)
      assert.is_nil(config.get())
    end)

    it('warns and stores nothing when `path` is missing', function()
      local notify = stub(vim, 'notify')

      config.set({})

      assert.stub(notify).was.called_at_least(1)
      assert.is_nil(config.get())
    end)

    it('strips a trailing slash', function()
      config.set({ path = '/home/user/notes/' })

      local conf = config.get()
      assert.are.equal('/home/user/notes', conf and conf.path)
    end)

    it('collapses redundant path segments', function()
      config.set({ path = '/home/user/./notes/../notes' })

      local conf = config.get()
      assert.are.equal('/home/user/notes', conf and conf.path)
    end)

    it('expands a leading ~', function()
      config.set({ path = '~/notes' })

      local conf = config.get()
      local path = (conf and conf.path) or ''
      assert.is_nil(path:find('^~'))
      assert.is_not_nil(path:find('/notes$'))
    end)
  end)

  describe('get', function()
    it('warns and returns nil before `set` has run', function()
      local notify = stub(vim, 'notify')

      assert.is_nil(config.get())
      assert.stub(notify).was.called_at_least(1)
    end)

    it('returns the configured path', function()
      config.set({ path = '/absolute/notes' })

      local conf = config.get()
      assert.are.equal('/absolute/notes', conf and conf.path)
    end)
  end)
end)
