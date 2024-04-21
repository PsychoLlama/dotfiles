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

-- Support <tab> completion
vim.api.nvim_set_keymap('i', '<tab>', 'editor#mappings#tab_completion(v:false)', {
  noremap = true,
  silent = true,
  expr = true,
})

vim.api.nvim_set_keymap('i', '<s-tab>', 'editor#mappings#tab_completion(v:true)', {
  noremap = true,
  silent = true,
  expr = true,
})

-- Interaction settings
vim.o.wildmenu = true
vim.o.wildmode = 'longest,list,full'
vim.o.inccommand = 'nosplit'
vim.o.wrapscan = false
vim.o.pumheight = 10
vim.o.pumblend = 30
vim.o.winblend = 30
vim.o.autoread = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.mouse = ''

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
vim.o.grepprg = 'rg -n'


-- Use <space> as the leader key.
vim.g.mapleader = ' '
vim.api.nvim_set_keymap('n', '<space>', '<nop>', {})
vim.api.nvim_set_keymap('v', '<space>', '<nop>', {})

-- Quick navigation
vim.api.nvim_set_keymap('n', '<leader>[', ':call editor#mappings#explore_current_dir()<cr>',
  { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>p', ':call editor#open_project_root()<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>a', '<Plug>(alternaut-toggle)', {})
vim.api.nvim_set_keymap('n', '<leader>f', ':Files<cr>', { noremap = true })
vim.api.nvim_set_keymap('n', '<leader>b', ':Buffers<cr>', { noremap = true })

-- Copilot.vim
vim.g.copilot_no_tab_map = true
vim.api.nvim_set_keymap('i', '<c-j>', 'copilot#Accept("\\<CR>")', { noremap = true, expr = true, silent = true })

-- coc.nvim
vim.keymap.set('n', 'gd', '<Plug>(coc-definition)')
vim.keymap.set('n', 'gy', '<Plug>(coc-type-definition)')
vim.keymap.set('n', 'gi', '<Plug>(coc-implementation)')
vim.keymap.set('n', 'gr', '<Plug>(coc-references)')
vim.keymap.set('i', '<cr>', [[coc#pum#visible() ? coc#pum#confirm() : "\<c-g>u\<cr>\<c-r>=coc#on_enter()\<cr>"]], {
  expr = true,
  noremap = true,
})

vim.keymap.set('n', '<leader>rn', '<Plug>(coc-rename)')

vim.keymap.set({ 'x', 'o' }, 'if', '<Plug>(coc-funcobj-i)')
vim.keymap.set({ 'x', 'o' }, 'af', '<Plug>(coc-funcobj-a)')
vim.keymap.set({ 'x', 'o' }, 'ic', '<Plug>(coc-classobj-i)')
vim.keymap.set({ 'x', 'o' }, 'ac', '<Plug>(coc-classobj-a)')

vim.keymap.set('n', '[g', '<Plug>(coc-diagnostic-prev)')
vim.keymap.set('n', ']g', '<Plug>(coc-diagnostic-next)')

vim.keymap.set('n', 'K', function()
  if vim.fn.CocAction('hasProvider', 'hover') then
    vim.fn.CocActionAsync('doHover')
  else
    vim.fn.feedkeys('K', 'in')
  end
end)

-- Misc
vim.api.nvim_set_keymap('n', '<esc>', ':nohlsearch<cr><esc>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>;', ':call editor#mappings#test()<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>m', ':MarkdownPreview<cr>', { noremap = true, silent = true })

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
  },
})

-- Color scheme (VS Code's One Dark Pro theme)
local onedark = require('onedarkpro')
local colors = require('onedarkpro.helpers').get_colors('onedark')

onedark.setup({
  options = {
    transparency = true,
  },
  colors = {
    onedark = {
      bg = "#1e1e1e",
    },
  },
  highlights = {
    -- Editor built-ins. See `:h highlight-groups` for details.
    CursorLineNr = {
      fg = colors.blue,
    },
    GitGutterAdd = {
      fg = colors.green,
    },
    GitGutterChange = {
      fg = colors.yellow,
    },
    GitGutterDelete = {
      fg = colors.red,
    },
  },
  styles = {
    comments = "italic",
  },
})

onedark.load()

-- Clear the cursor line. I only use it for the cursor line number.
vim.api.nvim_set_hl(0, 'CursorLine', {})

-- lualine.nvim
local lualine_theme = require('lualine.themes.onedark')

lualine_theme.normal.c.bg = nil

-- Disable fg mode colors for the branch name.
lualine_theme.normal.b.fg = nil
lualine_theme.insert.b.fg = nil
lualine_theme.visual.b.fg = nil
lualine_theme.command.b.fg = nil

require('lualine').setup({
  options = {
    theme = lualine_theme,
  },
  sections = {
    lualine_a = {},
    lualine_b = { 'branch' },
    lualine_c = {
      'filename',
      {
        'diagnostics',
        sources = { 'coc' },
      },
    },
    lualine_x = { 'filetype' },
    lualine_y = { 'progress' },
    lualine_z = { 'location' },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { 'filename' },
    lualine_x = { 'location' },
    lualine_y = {},
    lualine_z = {},
  },
})

-- Zettelkaesten
require('regal').setup({
  slip_box = "~/attic/slip-box",
})

-- Navitron
require('navitron').setup({
  -- Default options.
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
