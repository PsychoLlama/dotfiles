scriptencoding utf-8

set backspace=indent,eol,start
set wildmode=longest,list,full
set listchars=tab:··,trail:·
set backupdir=~/.vim/backup
set nrformats=octal,hex,bin
set clipboard+=unnamedplus
set fileformat=unix
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
set grepprg=ag
set expandtab
set incsearch
set linebreak
set smartcase
set tabstop=2
set autoread
set undofile
set wildmenu
set showcmd
set backup
set number
set mouse=
set list

nnoremap <space> <nop>
let g:mapleader = "\<space>"

" Add persistent undo.
let &undodir = expand('~/.vim/undodir')
if !isdirectory(&undodir)
  call system('mkdir -p ' . &undodir)
endif

filetype plugin indent on
func! s:resume_last_cursor_position() abort
  " There's no guarantee &filetype is ready.
  if expand('%:p:t') is# 'COMMIT_EDITMSG'
    return
  endif

  let l:last_line = line("'\"")
  if l:last_line > 1 && l:last_line < line('$')
    normal! g`"
  endif
endfunc

augroup resume_last_cursor_position
  autocmd!
  autocmd BufReadPost * call <SID>resume_last_cursor_position()
augroup END

augroup rando_file_settings
  autocmd!
  autocmd FileType netrw,gitcommit setlocal signcolumn=no
  autocmd FileType text,notes setlocal textwidth=78
  autocmd FileType help,man wincmd _
augroup END

" Macros
let @b = 'SbeforeEach(() => {jA;kkj'
let @d = "Sdescribe('', () => {jA;kkf'"
let @t = "Sit('', () => {jA;kkl"
let @c = "Sconsole.log('');hhh"
let @e = "othrow new Error('Failed to open pod bay doors.A;:w"
