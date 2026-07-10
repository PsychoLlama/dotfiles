-- Configured via the plugin manifest's `opts` (see `:help core.pkg`). The
-- slip box `path` is required; callers set it through `plugins.note-nvim.opts`.
return function(opts)
  require('note').setup(opts)
end
