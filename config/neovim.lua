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
vim.opt.completeopt = { "menu", "menuone", "noselect" }

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


-- NOTE: Some plugin-specific configs expect `mapleader` to already be set.
require('core.pkg').load(function(plugins)
  local pkg_overrides = vim.env.VIM_PLUGINS
  if pkg_overrides == nil then
    return plugins
  end

  --- @type table<string, string> Package name to package path
  local custom = vim.iter(vim.split(pkg_overrides, ';'))
      :fold({}, function(acc, override)
        local kv_pair = vim.split(override, ':')
        acc[kv_pair[1]] = kv_pair[2]

        return acc
      end)

  -- TODO: Replace the `VIM_PLUGINS` hack with something less horrible.
  return vim.iter(plugins)
      :map(function(plugin)
        if custom[plugin.name] then
          return vim.tbl_extend('force', plugin, {
            type = 'path',
            source = custom[plugin.name],
          })
        end

        return plugin
      end)
      :totable()
end)


-- Misc mappings
vim.api.nvim_set_keymap('n', '<leader>p', '<cmd>call editor#open_project_root()<cr>', {
  noremap = true,
  silent = true,
  desc = 'Open project root',
})

vim.api.nvim_set_keymap('n', '<esc>', '<cmd>nohlsearch<cr><esc>', {
  noremap = true,
  silent = true,
  desc = 'Clear search highlights',
})

vim.api.nvim_set_keymap('n', '<leader>;', '<cmd>call editor#mappings#test()<cr>', {
  noremap = true,
  silent = true,
  desc = 'Run unit tests',
})

-- LSP Config
vim.lsp.handlers[vim.lsp.protocol.Methods.textDocument_hover] = vim.lsp.with(
  vim.lsp.handlers.hover,
  { border = "rounded" }
)

local lsp_autoformat_group = vim.api.nvim_create_augroup('lsp_autoformat', {})

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

    -- Copilot doesn't count. It doesn't have normal LSP mappings.
    if client.name == 'GitHub Copilot' or vim.b.lsp_initialized then
      return
    end

    local function bufmap(mode, lhs, rhs)
      vim.keymap.set(mode, lhs, rhs, {
        buffer = args.buf,
        desc = 'LSP',
      })
    end

    bufmap('n', 'K', vim.lsp.buf.hover)
    bufmap('n', 'gd', vim.lsp.buf.definition)
    bufmap('n', 'gD', vim.lsp.buf.declaration)
    bufmap('n', 'gi', vim.lsp.buf.implementation)
    bufmap('n', 'gy', vim.lsp.buf.type_definition)
    bufmap('n', 'gr', vim.lsp.buf.references)
    bufmap('n', 'gs', vim.lsp.buf.signature_help)
    bufmap('n', '<leader>rn', vim.lsp.buf.rename)
    bufmap('n', 'go', vim.lsp.buf.code_action)
    bufmap('n', 'gl', vim.diagnostic.open_float)
    bufmap('n', '[d', vim.diagnostic.goto_prev)
    bufmap('n', ']d', vim.diagnostic.goto_next)

    -- Format on save.
    -- TODO: Add a way to disable this.
    vim.api.nvim_create_autocmd('BufWritePre', {
      group = lsp_autoformat_group,
      buffer = args.buf,
      callback = function()
        vim.lsp.buf.format({
          filter = function(formatter)
            -- TypeScript LS bundles its own formatter. And it's useless.
            return formatter.name ~= 'typescript-language-server'
          end,
        })
      end,
    })

    vim.b.lsp_initialized = true
  end,
})

-- Zettelkaesten (hasn't graduated to a real plugin yet)
require('regal').setup({
  slip_box = "~/attic/slip-box",
})

vim.api.nvim_create_autocmd('FileType', {
  group = vim.api.nvim_create_augroup('docs-page-settings', {}),
  pattern = { "help", "man" },
  desc = 'Auto-expand documentation windows',
  callback = function()
    vim.cmd.wincmd('_')
  end,
})
