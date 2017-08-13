" Use Vim settings, not Vi.
" Side effects require this line to be set immediately.
set nocompatible

" Set the runtime path to include Vundle and initialize.
set runtimepath+=~/.vim/bundle/Vundle.vim

" Let the plugins begin!
call plug#begin('~/.vim/plugged')

Plug 'Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'}
Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
Plug 'vim-airline/vim-airline-themes'
Plug 'editorconfig/editorconfig-vim'
Plug 'vim-airline/vim-airline'
Plug 'airblade/vim-gitgutter'
Plug 'raichoo/purescript-vim', { 'for': 'purescript' }
Plug 'jiangmiao/auto-pairs'
Plug 'hashivim/vim-vagrant', { 'for': 'ruby' }
Plug 'lambdatoast/elm.vim', { 'for': 'elm' }
Plug 'davinche/godown-vim', { 'for': 'markdown' }
Plug 'jparise/vim-graphql', { 'for': 'graphql' }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'cespare/vim-toml'
Plug 'mbbill/undotree'
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'mattn/emmet-vim'
Plug 'eslint/eslint', { 'for': 'javascript' }
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'w0rp/ale'

" Themes
Plug 'tyrannicaltoucan/vim-quantum'
Plug 'liuchengxu/space-vim-dark'
Plug 'joshdick/onedark.vim'

call plug#end()

" Reload files changed outside vim.
set autoread

" Automatic indentation and filetype detection.
filetype plugin indent on

" Allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has('vms')
  set nobackup	" Do not keep a backup file, use versions instead.
else
  set backup	" Keep a backup file.
endif

" Keep 500 lines of command line history.
set history=500

" Show the cursor position all the time.
set ruler

" Display incomplete commands.
set showcmd

" Incrementally search.
set incsearch

" Don't wrap lines in the middle of a word.
set linebreak

" KILL THE MOUSE
set mouse=

"NERDTree config.
" Show hidden files in NERDTree by default.
let g:NERDTreeShowHidden=1

" Ignore these files.
let g:NERDTreeIgnore = ['\.swp$']

" Don't use Ex mode, use Q for formatting.
map Q gq

" CTRL-U in insert mode deletes a lot.
" Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
cnoremap <C-tab> <C-G>u<C-U>

" Persistent undo files.
let vimDir = '$HOME/.vim'
let &runtimepath.=','.vimDir

" Keep undo history across sessions by storing it in a file.

if has('persistent_undo')
  let myUndoDir = expand(vimDir . '/undodir')
  call system('mkdir ' . vimDir)
  call system('mkdir ' . myUndoDir)
  let &undodir = myUndoDir
  set undofile
endif

" Syntax highlighting! Yeah!
syntax on

" Something to do with autocommands.
if has('autocmd')

  " Put these in an autocmd group,
  " so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last
  " known cursor position.
  " Don't do it when the position is invalid or when
  " inside an event handler.
  " Also don't do it when the mark is in the first line,
  " that's the default position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

endif

" Convenient command to see the difference between
" the current buffer and the file it was loaded from,
" thus the changes you made.
" Only define it when not defined already.
if !exists(':DiffOrig')
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
    \ | wincmd p | diffthis
endif

" Indentation settings.
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set autoindent

" Editor style.
let g:onedark_termcolors=16
let g:netrw_banner=0
colorscheme onedark

set listchars=tab:··,trail:· " set listchars=tab:>-,trail:·
set list " Show invisibles.
set number " Add line numbers.
set nofoldenable " Disable code folding.
set shortmess+=I
set wildmenu
set wildmode=longest,list,full
set ignorecase
set laststatus=2
set backupcopy=yes
set backupdir=~/.vim/backup
set updatetime=250

" -- Keymaps --
noremap <silent> <c-h> :tabp<CR>
noremap <silent> <c-l> :tabn<CR>
noremap <silent> <c-n> :Te<CR>
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k

" -- Auto commands --
autocmd BufNewFile,BufRead .tmux.conf set syntax=sh

" -- Linting --
" A.L.E.
let g:ale_linters = {
  \ 'javascript': ['eslint'],
  \ 'html': ['htmlhint'],
  \ 'json': ['jsonlint'],
  \ 'yaml': ['yamllint'],
  \ 'sass': ['sasslint'],
  \ 'scss': ['sasslint'],
  \ 'rust': ['rustc'],
  \ 'css': ['stylelint'],
  \ 'vim': ['vint'],
  \ }

" JavaScript
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1

" Enable text completion.
call deoplete#enable()

" Attempt completion on tab.
" Copied from deoplete help page.
inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ deoplete#mappings#manual_complete()
  function! s:check_back_space() abort "{{{
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
  endfunction"}}}

if filereadable(glob('~/.custom-scripts/init.vim'))
  source ~/.custom-scripts/init.vim
endif
