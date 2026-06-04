-- Editing settings
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.opt.formatoptions = 'qc1orj'
vim.opt.fileformat = 'unix'
vim.opt.fileformats = { 'unix', 'dos', 'mac' }
vim.opt.textwidth = 78
vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 0
vim.opt.shiftround = true

-- Interaction settings
vim.opt.wildmenu = true
vim.opt.wildmode = { 'longest', 'list', 'full' }
vim.opt.inccommand = 'nosplit'
vim.opt.wrapscan = false
vim.opt.pumheight = 10
vim.opt.pumblend = 20
vim.opt.pumborder = 'rounded'
vim.opt.winborder = 'rounded'
vim.opt.winblend = 0
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.mouse = ''
vim.opt.completeopt = { 'menu', 'menuone', 'noselect' }

-- Display settings
vim.opt.showcmd = true
vim.opt.signcolumn = 'yes'
vim.opt.number = true
vim.opt.numberwidth = 3
vim.opt.list = true
vim.opt.listchars = 'tab:) ,trail:.'
vim.opt.foldenable = false
vim.opt.updatetime = 100
vim.opt.linebreak = true
vim.opt.cursorline = true
vim.opt.shortmess:append('I')

-- Storage settings
vim.opt.backupcopy = 'yes'
vim.opt.backup = true
vim.opt.backupdir = vim.fn.stdpath('state') .. '/backup'
vim.opt.undofile = true

-- Integrations
vim.opt.clipboard = 'unnamedplus'
vim.opt.grepprg = 'rg --vimgrep'

require('core.env').source_direnv_vimrc()

-- Built-in opt package shipped with Neovim. Registering it with core.pkg
-- gives us lazy loading on first use of `:Undotree` -- same contract as
-- nix-managed plugins.
require('core.pkg').add('nvim.undotree', {
  type = 'pack',
  source = vim.fs.joinpath(vim.env.VIMRUNTIME, 'pack/dist/opt/nvim.undotree'),
  defer = { cmd = 'Undotree' },
})

require('core.pkg').load()

-- Visual mode leader nop (normal mode set in Nix extraConfig before sourcing).
vim.api.nvim_set_keymap('v', '<space>', '<nop>', {})

-- Disable dumb markdown indents from `$VIMRUNTIME`.
vim.g.markdown_recommended_style = false

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
  vim.diagnostic.open_float()
end)

vim.diagnostic.config({
  jump = {
    on_jump = function()
      vim.diagnostic.open_float()
    end,
  },
})

-- Codelens is opt-in; enable globally so servers that publish lenses surface
-- them. The default `grx` binding runs the lens under the cursor.
vim.lsp.codelens.enable()

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

-- Enable treesitter highlighting for all filetypes with available parsers.
-- Built-in filetypes (lua, c, vim, markdown) have ftplugins that do this, but
-- third-party parsers don't.
vim.api.nvim_create_autocmd('FileType', {
  group = vim.api.nvim_create_augroup('treesitter-highlight', {}),
  callback = function(args)
    local lang = vim.treesitter.language.get_lang(args.match)

    if lang then
      pcall(vim.treesitter.start, args.buf, lang)
    end
  end,
})
