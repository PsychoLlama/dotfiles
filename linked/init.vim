scriptencoding utf-8

set runtimepath+=expand('~/.vim')
set backspace=indent,eol,start
set wildmode=longest,list,full
set listchars=tab:··,trail:·
set backupdir=~/.vim/backup
set backupcopy=yes
set updatetime=0
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
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'editorconfig/editorconfig-vim'
Plug 'roxma/vim-tmux-clipboard'
Plug 'vim-airline/vim-airline'
Plug 'mitermayer/vim-prettier', { 'for': ['javascript', 'graphql'] }
Plug 'airblade/vim-gitgutter'
Plug 'raichoo/purescript-vim', { 'for': 'purescript' }
Plug 'joshdick/onedark.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'hashivim/vim-vagrant', { 'for': 'ruby' }
Plug 'tpope/vim-commentary'
Plug 'davinche/godown-vim', { 'for': 'markdown' }
Plug 'jparise/vim-graphql', { 'for': 'graphql' }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'rstacruz/sparkup', { 'rtp': 'vim/' }
Plug 'chrisbra/csv.vim', { 'for': 'csv' }
Plug 'tpope/vim-repeat'
Plug 'cespare/vim-toml'
Plug 'mileszs/ack.vim'
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

let g:onedark_termcolors=16
colorscheme onedark

filetype plugin indent on
syntax on

noremap <silent> <c-h> :tabp<CR>
noremap <silent> <c-l> :tabn<CR>
noremap <silent> <c-n> :Te<CR>
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k

augroup resume_last_cursor_position
  autocmd!
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
augroup END

augroup rando_file_settings
  autocmd!
  autocmd BufNewFile,BufRead .eslintrc,.babelrc set filetype=json
  autocmd BufNewFile,BufRead .tmux.conf set filetype=sh
  autocmd FileType text setlocal textwidth=78
augroup END

" Reset all progress in the file.
function! s:git_checkout_file() abort
  let l:target = expand('%:p')
  let l:cmd = 'git checkout "' . l:target . '"'
  call system(l:cmd)
  execute 'edit!' . l:target
endfunction

command! Customize tabe ~/.config/nvim/init.vim
command! Gcheckout call s:git_checkout_file()

" Macros
let @b = 'SbeforeEach(() => {jA;kkj'
let @d = "Sdescribe('', () => {jA;kkf'"
let @t = "Sit('', () => {jA;kkl"
let @c = "Sconsole.log('');hhh"

" Plugin config
let g:prettier#config#bracket_spacing = 'true'
let g:prettier#exec_cmd_async = 1

let g:netrw_list_hide='^.DS_Store$,^.git/$,^\.\./$,^\./$'
let g:netrw_localrmdir='rm -r'
let g:netrw_use_errorwindow=0
let g:netrw_banner=0

let g:ackprg = 'ag --vimgrep --smart-case'
cnoreabbrev ag Ack

let g:ale_linters = {
\   'javascript': ['eslint'],
\   'bash': ['shellcheck'],
\   'zsh': ['shellcheck'],
\   'sh': ['shellcheck'],
\   'rust': ['rustc'],
\   'vim': ['vint'],
\ }

call deoplete#enable()
function! s:check_back_space() abort
  let l:col = col('.') - 1
  return !l:col || getline('.')[l:col - 1]  =~? '\s'
endfunction

function! s:deoplete_tab_completion() abort
  if pumvisible()
    return "\<C-n>"
  elseif s:check_back_space()
    return "\<TAB>"
  else
    return g:deoplete#mappings#manual_complete()
  endif
endfunction

inoremap <silent><expr><TAB> <SID>deoplete_tab_completion()

" Check for environment-specific vim settings.
if filereadable(expand('~/.custom-scripts/init.vim'))
  source ~/.custom-scripts/init.vim
endif
