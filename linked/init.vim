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

" Trim dangling newlines. Mostly from system commands.
function! s:chomp(string) abort
  return substitute(a:string, '\n$', '', '')
endfunction

" Get python3 executable location.
function! s:get_exe_path(prog) abort
  let l:path = system('which ' . shellescape(a:prog))
  return s:chomp(l:path)
endfunction

if executable('python3')
  let g:python3_host_prog = s:get_exe_path('python3')
endif

if executable('python2')
  let g:python2_host_prog = s:get_exe_path('python2')
endif

call plug#begin('~/.vim/plugged')
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'editorconfig/editorconfig-vim'
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
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'xolox/vim-notes'
Plug 'xolox/vim-misc'
Plug 'mxw/vim-jsx'
Plug 'w0rp/ale'
Plug 'autozimu/LanguageClient-neovim', {
      \   'do': 'bash install.sh',
      \   'branch': 'next',
      \ }

call plug#end()

let g:LanguageClient_serverCommands = {
      \   'rust': ['rustup', 'run', 'nightly', 'rls'],
      \ }

" Add persistent undo.
let &undodir = expand('~/.vim/undodir')
if !filereadable(&undodir)
  call system('mkdir -p ' . &undodir)
endif

let g:onedark_termcolors=16
colorscheme onedark

filetype plugin indent on
syntax on

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
  setlocal modifiable

  setfiletype diff
  silent r!git diff HEAD
  silent 1d

  setlocal nomodifiable nowriteany nobuflisted nonumber
        \ buftype=nowrite bufhidden=delete signcolumn=no listchars=

  wincmd h

  augroup close_diff_if_last_window
    autocmd!
    autocmd BufEnter * call <SID>close_diff_if_last_window()
  augroup END
endfunction

command! H call <SID>show_git_diff()

augroup rando_file_settings
  autocmd!
  autocmd BufNewFile,BufRead .eslintrc,.babelrc set filetype=json
  autocmd BufNewFile,BufRead .tmux.conf set filetype=sh
  autocmd FileType text,notes setlocal textwidth=78
  autocmd FileType gitcommit setlocal signcolumn=no | call <SID>show_git_diff()
  autocmd FileType netrw setlocal signcolumn=no
  autocmd FileType ale-preview wincmd J
  autocmd FileType help wincmd _
augroup END

" Reset all progress in the file.
function! s:git_reset_file() abort
  let l:file = fnameescape(expand('%:p'))
  let l:symlink_pointer = system('readlink ' . l:file)

  " Attempt to resolve symlinks.
  if len(l:symlink_pointer)
    let l:file = s:chomp(l:symlink_pointer)
    echom l:file
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

command! Gcheckout call s:git_reset_file()

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

" Plugin config
let g:jsx_ext_required = 0

let g:deoplete#enable_at_startup = 1

let g:netrw_list_hide='^.DS_Store$,^.git/$,^\.\./$,^\./$'
let g:netrw_localrmdir='rm -r'
let g:netrw_use_errorwindow=0
let g:netrw_banner=0

let g:ale_javascript_prettier_use_local_config = 1
let g:ale_sh_shellcheck_options = '-e SC2155'
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
let g:ale_linters.rust = ['rls']
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
  let l:filename = expand('%:p:t')
  let l:curdir = expand('%:p:h')
  execute 'edit ' . fnameescape(l:curdir)
  call search(l:filename)
endfunction

noremap <silent><C-h> :tabp<CR>
noremap <silent><C-l> :tabn<CR>
inoremap <silent><expr><TAB> <SID>tab_completion(0)
inoremap <silent><expr><S-TAB> <SID>tab_completion(1)
nnoremap <silent><leader>t :call <SID>toggle_copy_mode()<cr>
nnoremap <silent><leader>n :nohlsearch<cr>
nnoremap <silent><leader>c :call <SID>edit_vimrc()<cr>
nnoremap <silent><leader>a :ALEDetail<cr>
nnoremap <silent><leader>r :call <SID>explore_current_dir()<cr>
nnoremap <silent><C-n> :Texplore<cr>

" Highlight current line number.
set cursorline
highlight clear CursorLine
highlight CursorLineNr ctermfg=blue

" Check for environment-specific vim settings.
if filereadable(expand('~/dotfiles-env/init.vim'))
  source ~/dotfiles-env/init.vim
endif
