require('core.env').source_direnv_vimrc()
require('core.pkg').load()

-- Editing settings
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.o.formatoptions = 'qc1orj'
vim.o.fileformat = 'unix'
vim.opt.fileformats = { 'unix', 'dos', 'mac' }
vim.o.textwidth = 78
vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 0
vim.o.shiftround = true

-- Interaction settings
vim.o.wildmenu = true
vim.opt.wildmode = { 'longest', 'list', 'full' }
vim.o.inccommand = 'nosplit'
vim.o.wrapscan = false
vim.o.pumheight = 10
vim.o.pumblend = 20
vim.o.winblend = 0
vim.o.autoread = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.mouse = ''
vim.opt.completeopt = { 'menu', 'menuone', 'noselect' }

-- Display settings
vim.o.incsearch = true
vim.o.showcmd = true
vim.o.termguicolors = true
vim.opt.shortmess:append('I')
vim.o.signcolumn = 'yes'
vim.o.number = true
vim.o.numberwidth = 3
vim.o.list = true
vim.o.listchars = 'tab:) ,trail:.'
vim.o.foldenable = false
vim.o.updatetime = 100
vim.o.linebreak = true
vim.o.cursorline = true

-- Storage settings
vim.o.backupcopy = 'yes'
vim.o.backup = true
vim.o.backupdir = '/tmp'
vim.o.undofile = true
vim.o.undodir = vim.fs.normalize('~/.vim/undo')
vim.o.history = 10000

-- Integrations
vim.o.clipboard = 'unnamedplus'
vim.o.grepprg = 'rg --vimgrep'

-- Use <space> as the leader key.
vim.g.mapleader = ' '
vim.api.nvim_set_keymap('n', '<space>', '<nop>', {})
vim.api.nvim_set_keymap('v', '<space>', '<nop>', {})

-- Misc mappings
vim.api.nvim_set_keymap(
  'n',
  '<leader>p',
  '<cmd>call editor#open_project_root()<cr>',
  {
    noremap = true,
    silent = true,
    desc = 'Open project root',
  }
)

vim.api.nvim_set_keymap('n', '<esc>', '<cmd>nohlsearch<cr><esc>', {
  noremap = true,
  silent = true,
  desc = 'Clear search highlights',
})

vim.api.nvim_set_keymap(
  'n',
  '<leader>;',
  '<cmd>call editor#mappings#test()<cr>',
  {
    noremap = true,
    silent = true,
    desc = 'Run unit tests',
  }
)

-- Diagnostics are sourced from both standalone linters and language servers.
vim.keymap.set('n', 'gl', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)

-- LSP Config
vim.lsp.handlers[vim.lsp.protocol.Methods.textDocument_hover] =
  vim.lsp.with(vim.lsp.handlers.hover, { border = 'rounded' })

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
    --- server doesn't support it. A good example is Copilot which supports
    --- almost nothing but is attached to most buffers.
    local function lspmap(binding, lsp_method, callback, opts)
      if has_mapping[lsp_method] then
        return
      end

      if not client.supports_method(lsp_method) then
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

    lspmap('K', P.textDocument_hover, vim.lsp.buf.hover)
    lspmap('<c-k>', P.textDocument_signatureHelp, vim.lsp.buf.signature_help)
    lspmap('gd', P.textDocument_definition, vim.lsp.buf.definition)
    lspmap('gi', P.textDocument_implementation, vim.lsp.buf.implementation)
    lspmap('gr', P.textDocument_references, vim.lsp.buf.references)
    lspmap('gy', P.textDocument_typeDefinition, vim.lsp.buf.type_definition)
    lspmap('<leader>rn', P.textDocument_rename, vim.lsp.buf.rename)
    lspmap('go', P.textDocument_codeAction, vim.lsp.buf.code_action)
  end,
})

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
