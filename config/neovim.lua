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
vim.o.autoread = true
vim.o.ignorecase = true
vim.o.smartcase = true

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
vim.o.updatetime = 0
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
vim.o.grepprg = 'rg -n'


-- Use <space> as the leader key.
vim.g.mapleader = ' '
vim.api.nvim_set_keymap('n', '<space>', '<nop>', {})
vim.api.nvim_set_keymap('v', '<space>', '<nop>', {})

-- Quick navigation
vim.api.nvim_set_keymap('n', '<leader>v', ':call editor#mappings#edit_vimrc()<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>r', ':call editor#mappings#explore_current_dir()<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>p', ':call editor#open_project_root()<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>a', '<Plug>(alternaut-toggle)', {})
vim.api.nvim_set_keymap('n', '<leader>f', ':Files!<cr>', { noremap = true })
vim.api.nvim_set_keymap('n', '<leader>s', ':Rg!<cr>', { noremap = true })

-- Support <tab> completion
vim.api.nvim_set_keymap('i', '<tab>', 'editor#mappings#tab_completion(v:false)', { noremap = true, silent = true, expr = true })
vim.api.nvim_set_keymap('i', '<s-tab>', 'editor#mappings#tab_completion(v:true)', { noremap = true, silent = true, expr = true })

-- Misc
vim.api.nvim_set_keymap('n', '<esc>', ':nohlsearch<cr><esc>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>;', ':call editor#mappings#test()<cr>', { noremap = true, silent = true })


-- ALE engine
vim.g.ale_sign_warning = '!'
vim.g.ale_sign_error = 'x'
vim.g.ale_fix_on_save = true

vim.g.ale_javascript_prettier_use_local_config = 1
vim.g.ale_sh_shellcheck_options = '-e SC2155'

vim.g.ale_linters = {
  javascript = { 'eslint' },
  bash = { 'shellcheck' },
  sh = { 'shellcheck' },
  rust = { 'rls' },
  graphql = { 'gqlint' },
  vim = { 'vint' },
}

vim.g.ale_fixers = {
  javascript = { 'prettier' },
  typescript = { 'prettier' },
  terraform = { 'terraform' },
  tex = { 'latexindent' },
  rust = { 'rustfmt' },
  nix = { 'nixfmt' }
}

vim.g.ale_pattern_options = {
  ['.*/node_modules/.*'] = { ale_enabled = false },
}

-- Test/source file alternation
local test_conventions = {
  javascript = {
    file_naming_conventions = { '{name}.test.{ext}' },
    directory_naming_conventions = { '__tests__' },
    file_extensions = { 'js' },
  },

  typescript = {
    file_naming_conventions = { '{name}.test.{ext}' },
    directory_naming_conventions = { '__tests__' },
    file_extensions = { 'ts', 'tsx', 'js' },
  },

  python = {
    file_naming_conventions = { 'test_{name}.{ext}', '{name}.{ext}' },
    directory_naming_conventions = { 'tests' },
    file_extensions = { 'py' },
  },

  vim = {
    file_naming_conventions = { '{name}.{ext}' },
    directory_naming_conventions = { 'tests' },
    file_extensions = { 'vim', 'vader' },
  },
}

test_conventions['javascript.jsx'] = test_conventions.javascript
test_conventions['typescript.tsx'] = test_conventions.typescript
test_conventions.vader = test_conventions.vim

vim.g['alternaut#conventions'] = test_conventions

-- Tree-Sitter
require('nvim-treesitter.configs').setup({
  highlight = { enable = true },
  indent = { enable = true },
  textobjects = {
    swap = {
      enable = true,
      swap_next = {
        ['g>'] = '@parameter.inner',
      },
      swap_previous = {
        ['g<'] = '@parameter.inner',
      },
    },

    select = {
      enable = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
  },
})

-- Misc
vim.g['further#prefer_modules'] = true
vim.g['teleport#update_cwd'] = true
vim.g.jsx_ext_required = 0
vim.g.splitjoin_trailing_comma = true
vim.g.loaded_netrwPlugin = true

-- Finalize config after plugins load.
--
-- Lua doesn't support autocmd hooks yet.
-- See: https://github.com/neovim/neovim/pull/12378
vim.cmd([[
augroup settings
  autocmd!

  " Wait for plugins to initalize before setting the color scheme.
  autocmd VimEnter * call g:__init() | delfunc! g:__init

  " Automatically maximize documentation pages.
  autocmd FileType help,man wincmd _
augroup END

func! g:__init() abort
  colorscheme onedark

  highlight clear ALEWarningSign
  highlight ALEWarningSign ctermfg=gray

  highlight clear CursorLine
  highlight CursorLineNr ctermfg=blue
endfunc
]])
