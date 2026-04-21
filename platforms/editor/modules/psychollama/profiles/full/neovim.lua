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

-- Operations not yet supported by the Nix binding.
vim.opt.shortmess:append('I')
vim.opt.backupdir = vim.fn.stdpath('state') .. '/backup'

-- Visual mode leader nop (normal mode set in Nix extraConfig before sourcing).
vim.api.nvim_set_keymap('v', '<space>', '<nop>', {})

-- Disable dumb markdown indents from `$VIMRUNTIME`.
vim.g.markdown_recommended_style = false

-- Rounded border for diagnostic floats, matching the rest of the editor UI.
local DIAGNOSTIC_FLOAT_STYLE = { border = 'rounded' }

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
  vim.diagnostic.open_float(DIAGNOSTIC_FLOAT_STYLE)
end)

vim.diagnostic.config({
  jump = {
    on_jump = function()
      vim.diagnostic.open_float(DIAGNOSTIC_FLOAT_STYLE)
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
