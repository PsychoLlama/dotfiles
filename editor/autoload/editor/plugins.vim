scriptencoding utf-8

" Get python3 executable location.
function! s:get_exe_path(prog) abort
  let l:path = system('command -v ' . shellescape(a:prog))
  return substitute(l:path, '\n$', '', '')
endfunction

if executable('python3')
  let g:python3_host_prog = s:get_exe_path('python3')
endif

if executable('python2')
  let g:python2_host_prog = s:get_exe_path('python2')
endif

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
Plug 'PsychoLlama/further.vim'
Plug 'PsychoLlama/snake.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'Shougo/deoplete.nvim'
Plug 'PsychoLlama/vim-gol'
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
let g:ale_linters.graphql = ['gqlint']
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

" File browser config.
let g:netrw_list_hide='^.DS_Store$,^.git/$,^\.\./$,^\./$'
let g:netrw_localrmdir='rm -r'
let g:netrw_use_errorwindow=0
let g:netrw_banner=0

" Use vim-jsx for .js extensions too.
let g:jsx_ext_required = 0

" Injects a kernel rootkit which slowly destroys the computer.
let g:deoplete#enable_at_startup = 1

function! editor#plugins#Init() abort
  return v:true
endfunction

" Neovim's Buit-in system clipboard integration.
let g:clipboard = {
      \   'name': 'tmux',
      \   'copy': {
      \      '+': 'tmux load-buffer -',
      \      '*': 'tmux load-buffer -',
      \    },
      \   'paste': {
      \      '+': 'tmux save-buffer -',
      \      '*': 'tmux save-buffer -',
      \   },
      \   'cache_enabled': 1,
      \ }
