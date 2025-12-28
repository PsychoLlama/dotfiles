return function(opts)
  local companion = require('codecompanion')
  local adapters = require('codecompanion.adapters')

  -- If litellm is configured, create an adapter proxying to the local instance.
  if opts.litellm then
    local url = opts.litellm.url or 'http://localhost:4000'
    local api_key = opts.litellm.api_key or 'LITELLM_API_KEY'

    opts.adapters = opts.adapters or {}
    opts.adapters.http = opts.adapters.http or {}
    opts.adapters.http.litellm = function()
      return adapters.extend('openai_compatible', {
        env = {
          url = url,
          api_key = api_key,
          chat_url = '/v1/chat/completions',
        },
      })
    end

    -- Point all interactions to the litellm adapter.
    opts.interactions = opts.interactions or {}
    opts.interactions.chat = opts.interactions.chat or {}
    opts.interactions.chat.adapter = 'litellm'
    opts.interactions.inline = opts.interactions.inline or {}
    opts.interactions.inline.adapter = 'litellm'
    opts.interactions.cmd = opts.interactions.cmd or {}
    opts.interactions.cmd.adapter = 'litellm'

    opts.litellm = nil
  end

  companion.setup(opts)

  vim.cmd.cabbrev('CC', 'CodeCompanion')
  vim.keymap.set('n', '<leader>c', companion.toggle, {
    desc = 'Toggle CodeCompanion',
  })

  vim.api.nvim_create_autocmd('FileType', {
    group = vim.api.nvim_create_augroup('codecompanion', {}),
    pattern = 'codecompanion',
    callback = vim.schedule_wrap(function()
      vim.opt_local.number = false
      vim.opt_local.signcolumn = 'yes'
      vim.api.nvim_feedkeys('i', 'n', true)
    end),
  })
end
