-- Configured via the plugin manifest's `opts` (see `:help core.pkg`). The
-- slip box `path` is required; callers set it through `plugins.note-nvim.opts`.
return function(opts)
  local note = require('note')
  note.setup(opts)

  vim.keymap.set('n', '<leader>no', note.open, { desc = 'Open notes' })
  vim.keymap.set('n', '<leader>ni', note.create, { desc = 'Create note' })

  -- Rename is only meaningful inside the slip box, so bind it buffer-locally
  -- to notes under the note path rather than globally.
  if opts and opts.path then
    vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
      group = vim.api.nvim_create_augroup('note.nvim', { clear = true }),
      pattern = vim.fs.normalize(opts.path) .. '/*',
      desc = 'Enable the note rename keymap inside the slip box',
      callback = function(args)
        vim.keymap.set('n', '<leader>nr', note.rename, {
          buffer = args.buf,
          desc = 'Rename note',
        })
      end,
    })
  end
end
