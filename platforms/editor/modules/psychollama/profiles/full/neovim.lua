require('core.env').source_direnv_vimrc()
require('core.pkg').load()

-- Operation not yet supported by the Nix binding.
vim.opt.shortmess:append('I')

-- Use <space> as the leader key.
vim.g.mapleader = ' '
vim.api.nvim_set_keymap('n', '<space>', '<nop>', {})
vim.api.nvim_set_keymap('v', '<space>', '<nop>', {})

-- Disable dumb markdown indents from `$VIMRUNTIME`.
vim.g.markdown_recommended_style = false

-- Default border style. Managing this manually instead of `&winborder` to
-- keep settings isolated to LSP windows. Global settings affect plugins.
local LSP_FLOAT_STYLE = { border = 'rounded' }

-- Misc mappings
vim.keymap.set('n', '<leader>p', function()
  require('editor.navigation').open_project_root()
end, { desc = 'Open project root', silent = true })

vim.api.nvim_set_keymap('n', '<esc>', '<cmd>nohlsearch<cr><esc>', {
  noremap = true,
  silent = true,
  desc = 'Clear search highlights',
})

-- Diagnostics are sourced from both standalone linters and language servers.
vim.keymap.set('n', 'gl', function()
  vim.diagnostic.open_float(LSP_FLOAT_STYLE)
end)

vim.keymap.set('n', '[d', function()
  vim.diagnostic.jump({ count = -1, float = LSP_FLOAT_STYLE })
end)

vim.keymap.set('n', ']d', function()
  vim.diagnostic.jump({ count = 1, float = LSP_FLOAT_STYLE })
end)

-- LSP Config
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('lsp_settings', {}),
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)

    if not client then
      vim.notify(
        'Failed to initialize LSP mappings. Client not found.',
        vim.log.levels.ERROR
      )

      return
    end

    -- Only create mappings once per buffer per LSP method.
    local has_mapping = {}

    --- Create a keybinding for an LSP method, but only if the server supports
    --- the action. This prevents overriding default mappings like `K` if the
    --- server doesn't support it.
    local function lspmap(binding, lsp_method, callback, opts)
      if has_mapping[lsp_method] then
        return
      end

      if not client:supports_method(lsp_method, args.buf) then
        return
      end

      opts = opts or {}
      opts.mode = opts.mode or 'n'

      has_mapping[lsp_method] = true
      vim.keymap.set(opts.mode, binding, callback, {
        buffer = args.buf,
        desc = lsp_method,
      })
    end

    local P = vim.lsp.protocol.Methods

    lspmap('<c-k>', P.textDocument_signatureHelp, vim.lsp.buf.signature_help)
    lspmap('gd', P.textDocument_definition, vim.lsp.buf.definition)
    lspmap('gi', P.textDocument_implementation, vim.lsp.buf.implementation)
    lspmap('gr', P.textDocument_references, vim.lsp.buf.references)
    lspmap('gy', P.textDocument_typeDefinition, vim.lsp.buf.type_definition)
    lspmap('<leader>rn', P.textDocument_rename, vim.lsp.buf.rename)
    lspmap('go', P.textDocument_codeAction, vim.lsp.buf.code_action)
    lspmap('K', P.textDocument_hover, function()
      vim.lsp.buf.hover(LSP_FLOAT_STYLE)
    end)
  end,
})

-- The default horizontal split leaves your cursor on (IMO) the wrong side.
local function open_split()
  vim.cmd.wincmd('v')
  vim.cmd.wincmd('l')
end

vim.keymap.set('n', '<c-w>v', open_split)
vim.keymap.set('n', '<c-w><c-v>', open_split)

-- Zettelkaesten (hasn't graduated to a real plugin yet)
require('regal').setup({
  slip_box = '~/attic/slip-box',
})

vim.api.nvim_create_autocmd('FileType', {
  group = vim.api.nvim_create_augroup('docs-page-settings', {}),
  pattern = { 'help', 'man' },
  desc = 'Auto-expand documentation windows',
  callback = function()
    vim.cmd.wincmd('_')
  end,
})

vim.treesitter.query.add_predicate('is-not?', function()
  -- Many TS grammars ship queries that use `#is-not?` to detect locals. The
  -- errors make the buffer unusable. This is a stub to fail more gracefully.
  -- If Neovim adds support later, `add_predicate` will throw on startup.
  --
  -- See: https://github.com/neovim/neovim/issues/27521
end, {})
