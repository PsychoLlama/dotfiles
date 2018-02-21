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

" Trim dangling newlines. Mostly from system commands.
function! s:chomp(string) abort
  return substitute(a:string, '\n$', '', '')
endfunction

" A namespace for shared vimscript functions (mostly consumed from
" ~/dotfiles-env).
let g:llama = { 'utils': {} }

let s:dotfiles_dir = s:chomp(system('dotfiles dir'))
let s:plugin_config = s:dotfiles_dir . '/editor/plugins.vim'
let s:utilities = s:dotfiles_dir . '/editor/utils.vim'

" RELEASE THE (plugin) KRAKEN
execute 'source ' . fnameescape(s:plugin_config)
execute 'source ' . fnameescape(s:utilities)

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

  setlocal nomodifiable nowriteany nobuflisted nonumber listchars=
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

function! s:is_typing_word() abort
  let l:col = col('.') - 1

  " Require at least 2 chars of context.
  if l:col < 3
    return 0
  endif

  let l:prev_chars = getline('.')[l:col - 2:l:col]
  return l:prev_chars =~? '\w\{2}'
endfunction

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
    setlocal nonumber signcolumn=no
  else
    setlocal number signcolumn=yes
  endif
endfunction

function! s:edit_vimrc() abort
  let l:cmd = isdirectory(expand('%:p')) ? 'edit' : 'tabedit'
  let l:dotfiles = s:chomp(system('dotfiles dir'))
  execute l:cmd . ' ' . l:dotfiles . '/linked/init.vim'
endfunction

" :Rexplore only works if the file was opened via netrw.
function! s:explore_current_dir() abort
  if &filetype is# 'netrw'
    return
  endif

  let l:filename = expand('%:p:t')
  let l:curdir = expand('%:p:h')
  execute 'edit ' . fnameescape(l:curdir)
  call search(l:filename)
endfunction


" This habit must die.
nnoremap <silent><C-h> :tabprevious<CR>
nnoremap <silent><C-l> :tabnext<CR>

" Do this instead.
nnoremap <silent><tab> :tabnext<CR>
nnoremap <silent><S-tab> :tabprevious<CR>

inoremap <silent><expr><TAB> <SID>tab_completion(0)
inoremap <silent><expr><S-TAB> <SID>tab_completion(1)
nnoremap <silent><leader>t :call <SID>toggle_copy_mode()<cr>
nnoremap <silent><leader>n :nohlsearch<cr>
nnoremap <silent><leader>c :call <SID>edit_vimrc()<cr>
nnoremap <silent><leader>r :call <SID>explore_current_dir()<cr>
nnoremap <silent><C-n> :Texplore<cr>

" Check for environment-specific vim settings.
if filereadable(expand('~/dotfiles-env/init.vim'))
  source ~/dotfiles-env/init.vim
endif
