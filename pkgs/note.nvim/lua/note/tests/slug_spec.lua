local slug = require('note.slug')

describe('note.slug', function()
  describe('normalize_title', function()
    it('lowercases the title', function()
      assert.are.equal('hello', slug.normalize_title('HeLLo'))
    end)

    it('replaces spaces with hyphens', function()
      assert.are.equal('two-words', slug.normalize_title('two words'))
    end)

    it('replaces colons with hyphens', function()
      assert.are.equal('a-b', slug.normalize_title('a:b'))
    end)

    it('leaves an already-clean title untouched', function()
      assert.are.equal('already-clean', slug.normalize_title('already-clean'))
    end)

    it('keeps digits', function()
      assert.are.equal('note-42', slug.normalize_title('Note 42'))
    end)

    it('collapses runs of separators into a single hyphen', function()
      -- The colon and the following space are one run, not two hyphens.
      assert.are.equal(
        'meeting-notes',
        slug.normalize_title('Meeting: Notes')
      )
      assert.are.equal('a-b', slug.normalize_title('a   b'))
      assert.are.equal('a-b', slug.normalize_title('a - b'))
    end)

    it('trims leading and trailing separators', function()
      assert.are.equal('hi', slug.normalize_title('  hi  '))
      assert.are.equal('hello', slug.normalize_title('hello!'))
      assert.are.equal('hello', slug.normalize_title('---hello---'))
    end)

    it('strips a leading dot so the file is not hidden', function()
      assert.are.equal('hidden', slug.normalize_title('.hidden'))
      assert.are.equal('bashrc', slug.normalize_title('.bashrc'))
    end)

    it('neutralizes path separators', function()
      -- A slash must never survive into a single filename component.
      assert.are.equal('foo-bar', slug.normalize_title('foo/bar'))
      assert.are.equal('c-users', slug.normalize_title('C:\\Users'))
    end)

    it('neutralizes path traversal', function()
      assert.are.equal('etc-passwd', slug.normalize_title('../../etc/passwd'))
    end)

    it('replaces underscores with hyphens', function()
      assert.are.equal('foo-bar', slug.normalize_title('foo_bar'))
    end)

    it('collapses tabs and other whitespace', function()
      assert.are.equal('a-b', slug.normalize_title('a\tb'))
    end)

    it('handles arbitrary punctuation', function()
      assert.are.equal('what-now', slug.normalize_title('What?! (now)'))
    end)

    it(
      'falls back to "untitled" when nothing alphanumeric remains',
      function()
        assert.are.equal('untitled', slug.normalize_title(''))
        assert.are.equal('untitled', slug.normalize_title('!!!'))
        assert.are.equal('untitled', slug.normalize_title('---'))
        assert.are.equal('untitled', slug.normalize_title('   '))
      end
    )

    it('caps the slug length without leaving a trailing hyphen', function()
      -- 250 "a b" pairs would yield a 499-char slug uncapped.
      local title = string.rep('a b ', 250)
      local result = slug.normalize_title(title)
      assert.is_true(#result <= 200)
      assert.is_nil(result:match('%-$'))
    end)
  end)

  describe('make_filename', function()
    it('builds a <timestamp>-<slug>.md name', function()
      assert.are.equal(
        '1699900000-my-note.md',
        slug.make_filename(1699900000, 'My Note')
      )
    end)

    it('slugifies the title', function()
      assert.are.equal(
        '1699900000-meeting-notes.md',
        slug.make_filename(1699900000, 'Meeting: Notes')
      )
    end)

    it('still yields a valid name for a hostile title', function()
      assert.are.equal(
        '1699900000-etc-passwd.md',
        slug.make_filename(1699900000, '../../etc/passwd')
      )
    end)
  end)

  describe('rename_filename', function()
    it('keeps the timestamp prefix and extension, swaps the slug', function()
      assert.are.equal(
        '1699900000-new-title.md',
        slug.rename_filename('1699900000-old-title.md', 'New Title')
      )
    end)

    it('slugifies the new title', function()
      assert.are.equal(
        '1699900000-meeting-notes.md',
        slug.rename_filename('1699900000-old.md', 'Meeting: Notes')
      )
    end)

    it('falls back to no prefix when the name has none', function()
      assert.are.equal(
        'plain-note.md',
        slug.rename_filename('whatever.md', 'Plain Note')
      )
    end)
  end)
end)
