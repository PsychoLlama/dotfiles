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
vim.o.pumblend = 20
vim.o.winblend = 0
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

-- Copilot.vim
vim.g.copilot_no_tab_map = true
vim.api.nvim_set_keymap('i', '<c-j>', 'copilot#Accept("\\<CR>")', { noremap = true, expr = true, silent = true })

-- CopilotChat.nvim
require('CopilotChat').setup({
  -- Default options.
})

-- coc.nvim
vim.keymap.set('n', 'gd', '<plug>(coc-definition)')
vim.keymap.set('n', 'gy', '<plug>(coc-type-definition)')
vim.keymap.set('n', 'gi', '<plug>(coc-implementation)')
vim.keymap.set('n', 'gr', '<plug>(coc-references)')
vim.keymap.set('i', '<cr>', [[coc#pum#visible() ? coc#pum#confirm() : "\<c-g>u\<cr>\<c-r>=coc#on_enter()\<cr>"]], {
  expr = true,
  noremap = true,
})

vim.keymap.set('n', '<leader>rn', '<plug>(coc-rename)')

vim.keymap.set({ 'x', 'o' }, 'if', '<plug>(coc-funcobj-i)')
vim.keymap.set({ 'x', 'o' }, 'af', '<plug>(coc-funcobj-a)')
vim.keymap.set({ 'x', 'o' }, 'ic', '<plug>(coc-classobj-i)')
vim.keymap.set({ 'x', 'o' }, 'ac', '<plug>(coc-classobj-a)')

vim.keymap.set('n', 'ge', '<cmd>Telescope coc diagnostics<cr>', { silent = true })
vim.keymap.set('n', '[g', '<plug>(coc-diagnostic-prev)')
vim.keymap.set('n', ']g', '<plug>(coc-diagnostic-next)')

vim.keymap.set('n', 'K', function()
  if vim.fn.CocAction('hasProvider', 'hover') then
    vim.fn.CocActionAsync('doHover')
  else
    vim.fn.feedkeys('K', 'in')
  end
end)

-- Misc
vim.api.nvim_set_keymap('n', '<esc>', '<cmd>nohlsearch<cr><esc>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>;', '<cmd>call editor#mappings#test()<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>m', '<cmd>MarkdownPreview<cr>', { noremap = true, silent = true })

-- Telescope
require('telescope').setup({
  defaults = {
    path_display = { "filename_first" },
    layout_strategy = 'vertical',
    layout_config = {
      height = { padding = 0 },
      width = { padding = 0 },
    },
  },
})

-- Open all telescope actions.
vim.keymap.set('n', '<leader>t', function()
  require('telescope.builtin').builtin({
    include_extensions = true,
  })
end)

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

-- gitlinker.nvim
local gitlinker = require('gitlinker')

gitlinker.setup({
  -- mappings = nil,
})

vim.keymap.set('n', '<leader>l', function()
  gitlinker.get_buf_range_url(vim.fn.mode())
end)

-- TODO: Figure out why this doesn't work with `keymap.set(...)`.
vim.api.nvim_set_keymap('v', '<leader>l', "<cmd>lua require'gitlinker'.get_buf_range_url('v')<cr>", {
  noremap = true,
})

---@see https://github.com/nvim-treesitter/nvim-treesitter#adding-parsers
require('nvim-treesitter.parsers').get_parser_configs().nu = {
  -- Default settings.
}

-- TreeSJ
local treesj = require('treesj')

treesj.setup({
  use_default_keymaps = false,
})

vim.keymap.set('n', '<leader>j', function()
  treesj.toggle()
end)

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

pcall(function()
  -- Disable fg mode colors for the branch name.
  -- Unknown why, but sometimes these objects don't exist yet.
  lualine_theme.normal.b.fg = nil
  lualine_theme.insert.b.fg = nil
  lualine_theme.visual.b.fg = nil
  lualine_theme.command.b.fg = nil
end)

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
