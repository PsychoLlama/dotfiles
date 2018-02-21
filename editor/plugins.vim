scriptencoding utf-8

call plug#begin('~/.vim/plugged')

" Filetype plugins
Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'chrisbra/csv.vim', { 'for': 'csv' }
Plug 'jparise/vim-graphql'
Plug 'rust-lang/rust.vim'
Plug 'tpope/vim-markdown'
Plug 'othree/yajs.vim'
Plug 'mxw/vim-jsx'

" ALL HAIL TIM POPE
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

" Language Server Protocol client.
Plug 'autozimu/LanguageClient-neovim', {
      \   'do': 'bash install.sh',
      \   'branch': 'next',
      \ }

" Rando utils.
Plug 'davinche/godown-vim', { 'for': 'markdown' }
Plug 'PsychoLlama/vim-gol', { 'on': 'GOL' }
Plug 'PsychoLlama/further.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'Shougo/deoplete.nvim'
Plug 'mbbill/undotree'
Plug 'w0rp/ale'

" For the :Notes command.
Plug 'xolox/vim-notes'
Plug 'xolox/vim-misc'

" Style 🦄
Plug 'vim-airline/vim-airline'
Plug 'airblade/vim-gitgutter'
Plug 'joshdick/onedark.vim'

call plug#end()

" Color scheme
let g:onedark_termcolors=16
colorscheme onedark
syntax on

highlight clear ALEWarningSign
highlight ALEWarningSign ctermfg=gray

set cursorline
highlight clear CursorLine
highlight CursorLineNr ctermfg=blue

" Rust config.
let g:LanguageClient_serverCommands = {
      \   'rust': ['rustup', 'run', 'nightly', 'rls'],
      \ }

" Linting.
nnoremap <silent><leader>a :ALEDetail<cr>

let g:ale_javascript_prettier_use_local_config = 1
let g:ale_sh_shellcheck_options = '-e SC2155'
let g:ale_sign_warning = '!'
let g:ale_sign_error = 'x'
let g:ale_fix_on_save = 1

let g:ale_linters = {}
let g:ale_linters.javascript = ['eslint', 'flow']
let g:ale_linters.bash = ['shellcheck']
let g:ale_linters.python = ['pylint']
let g:ale_linters.sh = ['shellcheck']
let g:ale_linters.rust = ['rls']
let g:ale_linters.vim = ['vint']

let g:ale_fixers = {}
let g:ale_fixers.javascript = ['prettier']
let g:ale_fixers.rust = ['rustfmt']

augroup adjust_ale_preview_pane
  autocmd!
  autocmd FileType ale-preview wincmd J
augroup END
