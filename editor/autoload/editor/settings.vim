scriptencoding utf-8

set backspace=indent,eol,start
set wildmode=longest,list,full
set listchars=tab:··,trail:·
set backupdir=~/.vim/backup
set clipboard+=unnamedplus
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

" Get every non-backgrounded buffer object.
function! s:get_active_buffers() abort
  let l:buffers = getbufinfo()
  let l:visible_buffers = filter(l:buffers, {index, buffer -> buffer.loaded})

  return l:visible_buffers
endfunction

function! s:close_diff_if_last_window() abort
  if exists('b:is_diff_window') && len(s:get_active_buffers()) is 1
    exit
  endif
endfunction

" Add persistent undo.
let &undodir = expand('~/.vim/undodir')
if !filereadable(&undodir)
  call system('mkdir -p ' . &undodir)
endif

filetype plugin indent on
function! s:resume_last_cursor_position() abort
  " There's no guarantee &filetype is ready.
  if expand('%:p:t') is# 'COMMIT_EDITMSG'
    return
  endif

  let l:last_line = line("'\"")
  if l:last_line > 1 && l:last_line < line('$')
    normal! g`"
  endif
endfunction

augroup resume_last_cursor_position
  autocmd!
  autocmd BufReadPost * call <SID>resume_last_cursor_position()
augroup END

function! s:show_git_diff() abort
  let l:mount_point = winwidth('.') >= 72 * 2 ? 'L' : 'J'

  vsplit Diff
  let b:is_diff_window = v:true

  execute 'wincmd ' . l:mount_point
  setfiletype diff
  call setline(1, systemlist('git diff HEAD'))

  setlocal nomodifiable nowriteany nobuflisted nonumber listchars=tab:--
  setlocal buftype=nowrite bufhidden=delete signcolumn=no
  let l:focus_point = l:mount_point is# 'L' ? 'h' : 'k'
  execute 'wincmd ' . l:focus_point

  augroup close_diff_if_last_window
    autocmd!
    autocmd BufEnter * call <SID>close_diff_if_last_window()
  augroup END
endfunction

augroup rando_file_settings
  autocmd!
  autocmd FileType gitcommit setlocal signcolumn=no | call <SID>show_git_diff()
  autocmd BufNewFile,BufRead .eslintrc,.babelrc set filetype=json
  autocmd BufNewFile,BufRead .tmux.conf set filetype=sh
  autocmd FileType text,notes setlocal textwidth=78
  autocmd FileType netrw setlocal signcolumn=no
  autocmd FileType help,man wincmd _
augroup END

" Macros
let @b = 'SbeforeEach(() => {jA;kkj'
let @d = "Sdescribe('', () => {jA;kkf'"
let @t = "Sit('', () => {jA;kkl"
let @c = "Sconsole.log('');hhh"
let @e = "othrow new Error('Failed to open pod bay doors.A;:w"

function! editor#settings#Init() abort
  return v:true
endfunction

" Check for environment-specific vim settings.
if filereadable(expand('~/dotfiles-env/init.vim'))
  source ~/dotfiles-env/init.vim
endif
