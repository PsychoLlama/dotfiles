local frontmatter = require('note.frontmatter')

describe('note.frontmatter', function()
  describe('parse', function()
    it('returns nothing when there is no opening fence', function()
      local fields, size = frontmatter.parse({ 'title: nope', 'body' })

      assert.are.same({}, fields)
      assert.are.equal(0, size)
    end)

    it('returns nothing when the block is never closed', function()
      local fields, size = frontmatter.parse({ '---', 'title: nope' })

      assert.are.same({}, fields)
      assert.are.equal(0, size)
    end)

    it('parses the fields and reports the block size', function()
      local fields, size = frontmatter.parse({
        '---',
        'title: Hello',
        'createdAt: 2026-07-10T00:00:00Z',
        '---',
        '',
        'body',
      })

      assert.are.same({
        title = 'Hello',
        createdAt = '2026-07-10T00:00:00Z',
      }, fields)
      -- Lines 1-4: both fences plus the two fields.
      assert.are.equal(4, size)
    end)

    it('keeps colons that appear inside the value', function()
      local fields = frontmatter.parse({
        '---',
        'title: Meeting: Q3 Planning',
        '---',
      })

      assert.are.equal('Meeting: Q3 Planning', fields.title)
    end)
  end)

  describe('generate', function()
    it('wraps fields in fences using the canonical order', function()
      local lines = frontmatter.generate({
        createdAt = '2026-07-10T00:00:00Z',
        title = 'Hello',
      })

      assert.are.same({
        '---',
        'title: Hello',
        'createdAt: 2026-07-10T00:00:00Z',
        '---',
      }, lines)
    end)

    it('omits fields that are absent', function()
      local lines = frontmatter.generate({ title = 'Only Title' })

      assert.are.same({ '---', 'title: Only Title', '---' }, lines)
    end)

    it('preserves unknown fields in sorted order', function()
      local lines = frontmatter.generate({
        title = 'Hello',
        zeta = 'z',
        alpha = 'a',
      })

      assert.are.same({
        '---',
        'title: Hello',
        'alpha: a',
        'zeta: z',
        '---',
      }, lines)
    end)
  end)

  describe('round trip', function()
    it('regenerates a parsed block unchanged', function()
      local original = {
        '---',
        'title: Hello',
        'createdAt: 2026-07-10T00:00:00Z',
        '---',
      }

      local fields = frontmatter.parse(original)

      assert.are.same(original, frontmatter.generate(fields))
    end)
  end)
end)
