" Editing settings
set backspace=indent,eol,start
set formatoptions=qc1orj
set fileformat=unix
set fileformats=unix,dos,mac
set textwidth=78
set expandtab tabstop=2
set shiftwidth=0
set shiftround

" Interaction settings
set wildmenu wildmode=longest,list,full
set inccommand=nosplit
set nowrapscan
set pumheight=10
set autoread

" Display settings
set incsearch
set showcmd
set termguicolors
set shortmess+=I
set signcolumn=yes
set number numberwidth=3
set list listchars=tab:)\ ,trail:.
set nofoldenable
set updatetime=0 " TODO: Decouple from :GitGutter
set linebreak
set cursorline

" Storage settings
set backupcopy=yes
set backup backupdir=/tmp
set undofile undodir=~/.vim/undo
set history=1000

" Integrations
set clipboard=unnamedplus
set grepprg=rg\ -n


" Use <space> as the leader key.
nmap <space> <nop>
vmap <space> <nop>
let mapleader = "\<space>"


" Quick navigation
nnoremap <silent><leader>v :call editor#mappings#edit_vimrc()<cr>
nnoremap <silent><leader>r :call editor#mappings#explore_current_dir()<cr>
nnoremap <silent><leader>p :call editor#open_project_root()<cr>
nmap <leader>a <Plug>(alternaut-toggle)
nnoremap <leader>f :Files!<cr>

" Poor man's templating function.
nnoremap <leader>sc <esc>:call editor#sf#javascript#log_statement()<cr>f'

" Support <tab> completion
inoremap <silent><expr><tab> editor#mappings#tab_completion(v:false)
inoremap <silent><expr><s-tab> editor#mappings#tab_completion(v:true)

" Misc
nnoremap <silent><esc> :nohlsearch<cr><esc>
nnoremap <silent><leader>; :call editor#mappings#test()<cr>
nmap <leader>t <Plug>(clippy-toggle-clipboard-mode)


" ALE engine
let g:ale_sign_warning = '!'
let g:ale_sign_error = 'x'
let g:ale_fix_on_save = v:true

let g:ale_linters = {}
let g:ale_fixers = {}
let g:ale_pattern_options = {}

let g:ale_linters.javascript = ['eslint', 'flow']
let g:ale_fixers.javascript = ['prettier']
let g:ale_pattern_options['.*/node_modules/.*'] = { 'ale_enabled': v:false }
let g:ale_javascript_prettier_use_local_config = 1
let g:ale_fixers.typescript = g:ale_fixers.javascript

let g:ale_linters.bash = ['shellcheck']
let g:ale_sh_shellcheck_options = '-e SC2155'

let g:ale_linters.rust = ['rls']
let g:ale_fixers.rust = ['rustfmt']

let g:ale_linters.graphql = ['gqlint']
let g:ale_linters.sh = ['shellcheck']
let g:ale_linters.vim = ['vint']

" Test/source file alternation
let alternaut#conventions = {}
let alternaut#conventions['javascript.jsx'] = {
      \   'file_naming_conventions': ['{name}.test.{ext}'],
      \   'directory_naming_conventions': ['__tests__'],
      \   'file_extensions': ['js'],
      \ }

let alternaut#conventions['typescript'] = {
      \   'file_naming_conventions': ['{name}.test.{ext}'],
      \   'directory_naming_conventions': ['__tests__'],
      \   'file_extensions': ['ts', 'tsx', 'js'],
      \ }

let alternaut#conventions['python'] = {
      \   'file_naming_conventions': ['test_{name}.{ext}', '{name}.{ext}'],
      \   'directory_naming_conventions': ['tests'],
      \   'file_extensions': ['py'],
      \ }

let alternaut#conventions['vim'] = {
      \   'file_naming_conventions': ['{name}.{ext}'],
      \   'directory_naming_conventions': ['tests'],
      \   'file_extensions': ['vim', 'vader'],
      \ }

let alternaut#conventions['typescript.tsx'] = alternaut#conventions['typescript']
let alternaut#conventions['vader'] = alternaut#conventions['vim']

" Misc
let further#prefer_modules = v:true
let teleport#update_cwd = v:true
let g:jsx_ext_required = 0
let g:splitjoin_trailing_comma = v:true
let g:loaded_netrwPlugin = v:true


augroup settings
  autocmd!

  " Wait for plugins to initalize before setting the color scheme.
  autocmd VimEnter * call s:init()

  " Automatically maximize documentation pages.
  autocmd FileType help,man wincmd _
augroup END

func! s:init() abort
  colorscheme onedark

  highlight clear ALEWarningSign
  highlight ALEWarningSign ctermfg=gray

  highlight clear CursorLine
  highlight CursorLineNr ctermfg=blue
endfunc
