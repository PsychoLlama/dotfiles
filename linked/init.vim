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

let s:dotfiles_dir = s:chomp(system('dotfiles dir'))
let s:plugin_config = s:dotfiles_dir . '/editor/plugins.vim'

" RELEASE THE (plugin) KRAKEN
execute 'source ' . fnameescape(s:plugin_config)

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

" A namespace for shared vimscript functions (mostly consumed from
" ~/dotfiles-env).
let g:llama = { 'utils': {} }

function! g:llama.utils.GetActiveBuffers() abort
  let l:buffers = getbufinfo()
  let l:visible_buffers = filter(l:buffers, {index, buffer -> buffer.loaded})

  return l:visible_buffers
endfunction

function! s:close_diff_if_last_window() abort
  if exists('b:is_diff_window') && len(g:llama.utils.GetActiveBuffers()) is 1
    exit
  endif
endfunction

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

command! H call <SID>show_git_diff()

augroup rando_file_settings
  autocmd!
  autocmd FileType gitcommit setlocal signcolumn=no | call <SID>show_git_diff()
  autocmd BufNewFile,BufRead .eslintrc,.babelrc set filetype=json
  autocmd BufNewFile,BufRead .tmux.conf set filetype=sh
  autocmd FileType text,notes setlocal textwidth=78
  autocmd FileType netrw setlocal signcolumn=no
  autocmd FileType help wincmd _
augroup END

" Reset all progress in the file.
function! s:git_reset_file() abort
  let l:file = fnameescape(expand('%:p'))
  let l:symlink_pointer = system('readlink ' . l:file)

  " Attempt to resolve symlinks.
  if len(l:symlink_pointer)
    let l:file = s:chomp(l:symlink_pointer)
  endif

  " system(...) uses the cwd context. Not good if
  " you execute this from a different repo.
  let l:original_cwd = getcwd()
  cd %:p:h

  call system('git reset -- ' . l:file)
  call system('git checkout -- ' . l:file)

  execute 'cd! ' . fnameescape(l:original_cwd)

  silent edit!
  silent write
endfunction

command! Reset call s:git_reset_file()

function! g:llama.utils.SearchDirUpwards(dir, cb) abort
  if a:cb(a:dir)
    return a:dir
  endif

  let l:dir = fnamemodify(a:dir, ':h')

  " Science has gone too far.
  if l:dir is a:dir
    return v:null
  endif

  return g:llama.utils.SearchDirUpwards(l:dir, a:cb)
endfunction

function! g:llama.utils.FindProjectRoot() abort
  let l:current_dir = expand('%:p:h')
  let l:Has_pkg_json = {dir -> file_readable(dir . '/package.json')}
  let l:project = g:llama.utils.SearchDirUpwards(l:current_dir, l:Has_pkg_json)

  if l:project is v:null
    let l:project = l:current_dir
  endif

  return l:project
endfunction

function! s:open_node_repl() abort
  let l:project = g:llama.utils.FindProjectRoot()

  copen
  execute 'lcd ' . fnameescape(l:project)
  execute 'term node'
  normal! A
endfunction

command! Node call <SID>open_node_repl()

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

function! s:find_authors_for_range(start, end) abort
  let l:range = a:start . ',' . a:end
  let l:cmd = 'git blame --porcelain -L ' . l:range . ' -- ' . fnameescape(expand('%:p'))
  let l:my_name = s:chomp(system('git config user.name'))
  let l:blames = systemlist(l:cmd)

  let l:committers = filter(l:blames, {k, v -> v =~# '^committer '})
  let l:authors = map(l:committers, {k, v -> substitute(v, 'committer ', '', '')})
  let l:uniq_authors = {}

  for l:author in l:authors
    let l:actual_author = l:author ==? 'not committed yet' ? l:my_name : l:author
    let l:uniq_authors[l:actual_author] = l:actual_author
  endfor

  return values(l:uniq_authors)
endfunction

function! s:find_line_author() abort range
  let l:authors = s:find_authors_for_range(a:firstline, a:lastline)

  echo join(l:authors, "\n")
endfunction

command! -range Author <line1>,<line2>call <SID>find_line_author()

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
