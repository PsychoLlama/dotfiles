local match = require('luassert.match')
local stub = require('luassert.stub')

local env = require('core.env')
local permission = require('core.env._permission')

describe('core.env', function()
  local snapshot

  before_each(function()
    snapshot = assert:snapshot()
  end)

  after_each(function()
    snapshot:revert()
  end)

  describe('setup', function()
    it('forwards trusted prefixes to the permission layer', function()
      local set = stub(permission, 'set_trusted_prefixes')

      env.setup({ trusted_prefixes = { '/projects' } })

      assert.stub(set).was.called_with({ '/projects' })
    end)

    it('defaults to an empty prefix list', function()
      local set = stub(permission, 'set_trusted_prefixes')

      env.setup()

      assert.stub(set).was.called_with({})
    end)
  end)

  describe('source_direnv_vimrc', function()
    it('does nothing when the env var is unset', function()
      stub(os, 'getenv', nil)
      local load = stub(env, 'load')

      env.source_direnv_vimrc()

      assert.stub(load).was.not_called()
    end)

    it('loads the file the env var points at', function()
      stub(os, 'getenv', '/foo/vimrc')
      local load = stub(env, 'load')

      env.source_direnv_vimrc()

      assert.stub(load).was.called(1)
      assert.stub(load).was.called_with('/foo/vimrc', match.is_table())
    end)

    it('loads every colon-separated path', function()
      stub(os, 'getenv', '/foo/vimrc:/bar/vimrc')
      local load = stub(env, 'load')

      env.source_direnv_vimrc()

      assert.stub(load).was.called(2)
      assert.stub(load).was.called_with('/foo/vimrc', match.is_table())
      assert.stub(load).was.called_with('/bar/vimrc', match.is_table())
    end)
  end)

  describe('load', function()
    it('sources the file when permission is granted', function()
      stub(permission, 'check_memory_or_ask', 'allow')
      local source = stub(vim.cmd, 'source')

      env.load('/foo/vimrc', {})

      assert.stub(source).was.called_with('/foo/vimrc')
    end)

    it('does not source the file when permission is denied', function()
      stub(permission, 'check_memory_or_ask', 'deny')
      local source = stub(vim.cmd, 'source')

      env.load('/foo/vimrc', {})

      assert.stub(source).was.not_called()
    end)

    it('warns instead of throwing when sourcing fails', function()
      stub(permission, 'check_memory_or_ask', 'allow')
      stub(vim.cmd, 'source', function()
        error('boom')
      end)
      local notify = stub(vim, 'notify')

      assert.has_no_error(function()
        env.load('/foo/vimrc', {})
      end)
      assert
        .stub(notify).was
        .called_with(match.is_string(), vim.log.levels.WARN)
    end)
  end)
end)
