scriptencoding utf-8

set backspace=indent,eol,start
set wildmode=longest,list,full
set listchars=tab:··,trail:·
set backupdir=~/.vim/backup
set backupcopy=yes
set signcolumn=yes
set numberwidth=2
set softtabstop=2
set nofoldenable
set updatetime=0
set shiftwidth=2
set shortmess+=I
set laststatus=2
set pumheight=10
set history=500
set autoindent
set ignorecase
set shiftround
set expandtab
set linebreak
set incsearch
set tabstop=2
set autoread
set undofile
set wildmenu
set showcmd
set backup
set number
set mouse=
set list

" Get python3 executable location.
if executable('python3')
  let g:python3_host_prog = substitute(system('which python3'), '\n$', '', '')
endif

call plug#begin('~/.vim/plugged')
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'editorconfig/editorconfig-vim'
Plug 'roxma/vim-tmux-clipboard'
Plug 'PsychoLlama/further.vim'
Plug 'vim-airline/vim-airline'
Plug 'airblade/vim-gitgutter'
Plug 'joshdick/onedark.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'hashivim/vim-vagrant', { 'for': 'ruby' }
Plug 'Shougo/deoplete.nvim'
Plug 'tpope/vim-commentary'
Plug 'davinche/godown-vim', { 'for': 'markdown' }
Plug 'PsychoLlama/vim-gol', { 'on': 'GOL' }
Plug 'jparise/vim-graphql', { 'for': 'graphql' }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'tpope/vim-surround'
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'chrisbra/csv.vim', { 'for': 'csv' }
Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'tpope/vim-repeat'
Plug 'mileszs/ack.vim', { 'on': 'Ack' }
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'xolox/vim-notes'
Plug 'xolox/vim-misc'
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
  autocmd FileType netrw setlocal signcolumn=no
  autocmd FileType text,notes setlocal textwidth=78
  autocmd FileType gitcommit setlocal signcolumn=no
augroup END

" Reset all progress in the file.
function! s:git_checkout_file() abort
  let l:target = expand('%:p')
  let l:cmd = 'git checkout "' . l:target . '"'
  call system(l:cmd)
  execute 'edit!' . l:target
endfunction

command! Gcheckout call s:git_checkout_file()

" Macros
let @b = 'SbeforeEach(() => {jA;kkj'
let @d = "Sdescribe('', () => {jA;kkf'"
let @t = "Sit('', () => {jA;kkl"
let @c = "Sconsole.log('');hhh"
let @e = "othrow new Error('Failed to open pod bay doors.A;:w"

" Plugin config
let g:deoplete#enable_at_startup = 1

let g:netrw_list_hide='^.DS_Store$,^.git/$,^\.\./$,^\./$'
let g:netrw_localrmdir='rm -r'
let g:netrw_use_errorwindow=0
let g:netrw_banner=0

let g:ackprg = 'ag --vimgrep --smart-case'
cnoreabbrev ag Ack

let g:ale_javascript_prettier_use_local_config = 1
let g:ale_sign_warning = '!'
let g:ale_sign_error = 'x'
let g:ale_fix_on_save = 1

highlight clear ALEWarningSign
highlight ALEWarningSign ctermfg=gray

let g:ale_fixers = {}
let g:ale_fixers.javascript = ['prettier']

let g:ale_linters = {}
let g:ale_linters.javascript = ['eslint', 'flow']
let g:ale_linters.bash = ['shellcheck']
let g:ale_linters.python = ['pylint']
let g:ale_linters.sh = ['shellcheck']
let g:ale_linters.rust = ['rustc']
let g:ale_linters.vim = ['vint']

function! s:is_typing_word() abort
  let l:col = col('.') - 1

  " Require at least 2 chars of context.
  if l:col < 3
    return 0
  endif

  let l:prev_chars = getline('.')[l:col - 2:l:col]
  return l:prev_chars =~? '\w\{2}'
endfunction

let g:last_cursor_position = []

function! s:tab_completion(shifting) abort
  if pumvisible()
    if a:shifting
      return "\<C-p>"
    endif

    return "\<C-n>"
  endif

  if s:is_typing_word()
    return "\<C-n>"
  endif

  return "\t"
endfunction

function! s:toggle_copy_mode() abort
  if &number
    setlocal nonumber
    setlocal signcolumn=no
  else
    setlocal number
    setlocal signcolumn=yes
  endif
endfunction

noremap <silent><C-h> :tabp<CR>
noremap <silent><C-l> :tabn<CR>
inoremap <silent><expr><TAB> <SID>tab_completion(0)
inoremap <silent><expr><S-TAB> <SID>tab_completion(1)
nnoremap <silent><leader>t :call <SID>toggle_copy_mode()<cr>
nnoremap <silent><leader>n :nohlsearch<cr>
nnoremap <silent><leader>c :tabe ~/.config/nvim/init.vim<cr>
nnoremap <silent><leader>a :ALEDetail<cr>
nnoremap <silent><leader>r :Rexplore<cr>
nnoremap <silent><C-n> :Texplore<cr>

" Highlight current line number.
set cursorline
highlight clear CursorLine
highlight CursorLineNr ctermfg=blue

" Check for environment-specific vim settings.
if filereadable(expand('~/.custom-scripts/init.vim'))
  source ~/.custom-scripts/init.vim
endif
