-- Editing settings
vim.o.backspace = 'indent,eol,start'
vim.o.formatoptions = 'qc1orj'
vim.o.fileformat = 'unix'
vim.o.fileformats = 'unix,dos,mac'
vim.o.textwidth = 78
vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 0
vim.o.shiftround = true

-- Interaction settings
vim.o.wildmenu = true
vim.o.wildmode = 'longest,list,full'
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
vim.o.shortmess = vim.go.shortmess .. 'I'
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
vim.o.undodir = os.getenv('HOME') .. '/.vim/undo'
vim.o.history = 1000

-- Integrations
vim.o.clipboard = 'unnamedplus'
vim.o.grepprg = 'rg --vimgrep'


-- Use <space> as the leader key.
vim.g.mapleader = ' '
vim.api.nvim_set_keymap('n', '<space>', '<nop>', {})
vim.api.nvim_set_keymap('v', '<space>', '<nop>', {})

-- Quick navigation
vim.api.nvim_set_keymap('n', '<leader>[', '', {
  noremap = true,
  silent = true,
  callback = function()
    if vim.tbl_contains({ 'navitron', 'netrw' }, vim.o.filetype) then
      return
    end

    local current_file = vim.fn.expand('%:p')
    local parent_directory = vim.fn.fnamemodify(current_file, ':p:h')
    require('navitron').open(parent_directory)
  end
})

vim.api.nvim_set_keymap('n', '<leader>p', '<cmd>call editor#open_project_root()<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>a', '<plug>(alternaut-toggle)', {})
vim.api.nvim_set_keymap('n', '<leader>f', '<cmd>Telescope find_files<cr>', { noremap = true })
vim.api.nvim_set_keymap('n', '<leader>b', '<cmd>Telescope buffers<cr>', { noremap = true })

-- Misc
vim.api.nvim_set_keymap('n', '<esc>', '<cmd>nohlsearch<cr><esc>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>;', '<cmd>call editor#mappings#test()<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>m', '<cmd>MarkdownPreview<cr>', { noremap = true, silent = true })

-- Color scheme (VS Code's One Dark Pro theme)
local onedark = require('onedarkpro')
local odp_helpers = require('onedarkpro.helpers')
local colors = odp_helpers.get_colors('onedark')
local bg_color = '#1e1e1e'

onedark.setup({
  options = {
    transparency = true,
  },
  colors = {
    onedark = {
      bg = bg_color,
    },
  },
  highlights = {
    -- Editor built-ins. See `:h highlight-groups` for details.
    CursorLineNr = {
      fg = colors.blue,
    },

    FloatTitle = {
      bg = bg_color,
      fg = colors.blue,
    },
  },
  styles = {
    comments = "italic",
  },
})

onedark.load()

-- Clear the cursor line. I only use it for the cursor line number.
vim.api.nvim_set_hl(0, 'CursorLine', {})

-- LSP Config
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
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

-- Zettelkaesten
require('regal').setup({
  slip_box = "~/attic/slip-box",
})

-- Misc
vim.g['teleport#update_cwd'] = true
vim.g.jsx_ext_required = 0
vim.g.splitjoin_trailing_comma = true

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = vim.api.nvim_create_augroup('settings', {}),
  pattern = { "help", "man" },
  command = "wincmd _",
})

vim.api.nvim_set_hl(0, 'FloatBorder', {
  bg = bg_color,
  fg = colors.black,
  blend = 20,
})

vim.api.nvim_set_hl(0, 'FloatTitle', {
  fg = colors.blue,
  bg = bg_color,
  blend = 20,
})

vim.api.nvim_set_hl(0, 'NormalFloat', {
  bg = bg_color,
  blend = 20,
})

-- nvim-cmp selected option.
vim.api.nvim_set_hl(0, 'PmenuSel', {
  bg = odp_helpers.lighten('bg', 10, 'onedark'),
  fg = 'NONE',
})
