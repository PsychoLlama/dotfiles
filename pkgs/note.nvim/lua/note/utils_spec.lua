local utils = require('note.utils')

describe('note.utils', function()
  describe('normalize_title', function()
    it('lowercases the title', function()
      assert.are.equal('hello', utils.normalize_title('HeLLo'))
    end)

    it('replaces spaces with hyphens', function()
      assert.are.equal('two-words', utils.normalize_title('two words'))
    end)

    it('replaces colons with hyphens', function()
      assert.are.equal('a-b', utils.normalize_title('a:b'))
    end)

    it('replaces each separator without collapsing runs', function()
      -- The colon and the space each map to a hyphen independently.
      assert.are.equal(
        'meeting--notes',
        utils.normalize_title('Meeting: Notes')
      )
    end)

    it('leaves an already-clean title untouched', function()
      assert.are.equal(
        'already-clean',
        utils.normalize_title('already-clean')
      )
    end)
  end)
end)
