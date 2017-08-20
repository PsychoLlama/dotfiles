set runtimepath+=expand('~/.vim')
set backspace=indent,eol,start
set wildmode=longest,list,full
set listchars=tab:··,trail:·
set backupdir=~/.vim/backup
set backupcopy=yes
set updatetime=250
set softtabstop=2
set nofoldenable
set shiftwidth=2
set shortmess+=I
set laststatus=2
set history=500
set ignorecase
set autoindent
set expandtab
set tabstop=2
set incsearch
set linebreak
set undofile
set wildmenu
set autoread
set showcmd
set backup
set number
set mouse=
set list

call plug#begin('~/.vim/plugged')
Plug 'Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'}
Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
Plug 'vim-airline/vim-airline-themes'
Plug 'editorconfig/editorconfig-vim'
Plug 'vim-airline/vim-airline'
Plug 'airblade/vim-gitgutter'
Plug 'raichoo/purescript-vim', { 'for': 'purescript' }
Plug 'joshdick/onedark.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'hashivim/vim-vagrant', { 'for': 'ruby' }
Plug 'lambdatoast/elm.vim', { 'for': 'elm' }
Plug 'davinche/godown-vim', { 'for': 'markdown' }
Plug 'jparise/vim-graphql', { 'for': 'graphql' }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'tpope/vim-repeat'
Plug 'cespare/vim-toml'
Plug 'mbbill/undotree'
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'mattn/emmet-vim'
Plug 'eslint/eslint', { 'for': 'javascript' }
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'w0rp/ale'
call plug#end()

" Add persistent undo.
let &undodir = expand('~/.vim/undodir')
if ! filereadable(&undodir)
  call system('mkdir -p ' . &undodir)
endif

" Always jump to the last known cursor position.
autocmd BufReadPost *
  \ if line("'\"") > 1 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif
augroup END

let g:onedark_termcolors=16
let g:netrw_banner=0

filetype plugin indent on
colorscheme onedark
syntax on

noremap <silent> <c-h> :tabp<CR>
noremap <silent> <c-l> :tabn<CR>
noremap <silent> <c-n> :Te<CR>
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k

autocmd BufNewFile,BufRead .tmux.conf set syntax=sh
autocmd BufNewFile,BufRead .eslintrc,.babelrc set syntax=json
autocmd FileType text setlocal textwidth=78

command! Customize tabe ~/.config/nvim/init.vim

" Plugin config
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'json': ['jsonlint'],
\   'yaml': ['yamllint'],
\   'rust': ['rustc'],
\   'vim': ['vint'],
\ }

call deoplete#enable()
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ deoplete#mappings#manual_complete()

" Check for environment-specific vim settings.
if filereadable(glob('~/.custom-scripts/init.vim'))
  source ~/.custom-scripts/init.vim
endif
