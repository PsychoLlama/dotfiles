local M = {}

--- Fallback slug for titles with no alphanumeric characters, so we never
--- produce an empty (or hidden, or all-separator) filename component.
local FALLBACK_SLUG = 'untitled'

--- Cap on slug length. A path component tops out at 255 bytes on the
--- filesystems we target; capping the slug well under that leaves room for the
--- `<timestamp>-` prefix and `.md` extension no matter how long the title is.
local MAX_SLUG_LENGTH = 200

--- Slugify a note title into a single filesystem-safe filename component.
---
--- This is the one place titles are escaped for use on disk, so it must be
--- total: every ASCII input maps to a safe slug. It lowercases the title,
--- reduces each run of non-alphanumeric characters to a single hyphen, and
--- trims hyphens from both ends. The result contains only `[a-z0-9-]`, so it
--- has no directory separators, no leading dot (which would hide the file),
--- and no path-traversal `..`. Titles with no alphanumerics fall back to
--- `untitled`, and over-long titles are truncated so the filename can't blow
--- past the filesystem's name-length limit.
--- @param title string
--- @return string
function M.normalize_title(title)
  local slug = title:lower():gsub('[^%w]+', '-')
  slug = slug:gsub('^%-+', ''):gsub('%-+$', '')

  if #slug > MAX_SLUG_LENGTH then
    -- Re-trim: truncation may have left a trailing hyphen mid-run.
    slug = slug:sub(1, MAX_SLUG_LENGTH):gsub('%-+$', '')
  end

  if slug == '' then
    return FALLBACK_SLUG
  end
  return slug
end

--- Build the filename for a new note: `<timestamp>-<slug>.md`. The leading
--- timestamp fixes the note's identity and sort order; the slug is derived
--- from its title. Centralizes filename construction so new notes and renames
--- agree on shape.
--- @param timestamp integer Seconds since the epoch (the note's identity).
--- @param title string
--- @return string
function M.make_filename(timestamp, title)
  return timestamp .. '-' .. M.normalize_title(title) .. '.md'
end

--- Swap the slug of a note filename for one derived from `new_title`, keeping
--- the leading `<timestamp>-` prefix and the extension so the note's identity
--- and sort order are preserved.
--- @param basename string e.g. "1699900000-old-title.md"
--- @param new_title string
--- @return string
function M.rename_filename(basename, new_title)
  local prefix = basename:match('^(%d+%-)') or ''
  local ext = basename:match('(%.[^.]+)$') or '.md'
  return prefix .. M.normalize_title(new_title) .. ext
end

return M
