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

" A namespace for shared vimscript functions (mostly consumed from
" ~/dotfiles-env).
let g:llama = { 'utils': {} }

" Trim dangling newlines. Mostly from system commands.
function! g:llama.utils.chomp(string) abort
  return substitute(a:string, '\n$', '', '')
endfunction

" Save keystrokes!
let s:chomp = g:llama.utils.chomp

let s:dotfiles_dir = s:chomp(system('dotfiles dir'))
let s:plugin_config = s:dotfiles_dir . '/editor/plugins.vim'
let s:utilities = s:dotfiles_dir . '/editor/utils.vim'
let s:mappings = s:dotfiles_dir . '/editor/mappings.vim'

" RELEASE THE (plugin) KRAKEN
execute 'source ' . fnameescape(s:plugin_config)
execute 'source ' . fnameescape(s:utilities)
execute 'source ' . fnameescape(s:mappings)

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

augroup resume_last_cursor_position
  autocmd!
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") && &filetype != 'gitcommit' |
    \   exe "normal! g`\"" |
    \ endif
augroup END

function! s:show_git_diff() abort
  vsplit new
  let b:is_diff_window = v:true

  wincmd L
  setfiletype diff
  silent r!git diff HEAD
  silent 1d

  setlocal nomodifiable nowriteany nobuflisted nonumber listchars=tab:--
  setlocal buftype=nowrite bufhidden=delete signcolumn=no
  wincmd h

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
  autocmd FileType help wincmd _
augroup END

" Macros
let @b = 'SbeforeEach(() => {jA;kkj'
let @d = "Sdescribe('', () => {jA;kkf'"
let @t = "Sit('', () => {jA;kkl"
let @c = "Sconsole.log('');hhh"
let @e = "othrow new Error('Failed to open pod bay doors.A;:w"

" Check for environment-specific vim settings.
if filereadable(expand('~/dotfiles-env/init.vim'))
  source ~/dotfiles-env/init.vim
endif
