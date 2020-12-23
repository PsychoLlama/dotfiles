" Editing settings
set backspace:indent,eol,start
set formatoptions:qc1orj
set fileformat:unix
set fileformats:unix,dos,mac
set textwidth:78
set expandtab tabstop:2
set shiftwidth:0
set shiftround

" Interaction settings
set wildmenu wildmode:longest,list,full
set inccommand:nosplit
set nowrapscan
set pumheight:10
set autoread

" Display settings
set incsearch
set showcmd
set termguicolors
set shortmess+=I
set signcolumn:yes
set number numberwidth:3
set list listchars:tab:)\ ,trail:.
set nofoldenable
set updatetime:0 " TODO: Decouple from :GitGutter
set linebreak
set laststatus:1
set cursorline

" Storage settings
set backupcopy:yes
set backup backupdir:/tmp
set undofile undodir:~/.vim/undo
set history:1000

" Integrations
set clipboard:unnamedplus
set grepprg:rg\ -n
set mouse:vin
set mousemodel:extend


" Use <space> as the leader key.
nmap <space> <nop>
let mapleader = "\<space>"


" Automatically maximize documentation pages.
augroup help_pages
  autocmd!
  autocmd FileType help,man wincmd _
augroup END


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
nnoremap <silent><leader>t :call editor#mappings#toggle_copy_mode()<cr>


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

" Neovim's built-in system clipboard integration.
if !empty($TMUX)
  let s:tmux_copy = 'tmux load-buffer -'
  let s:tmux_paste = 'tmux save-buffer -'

  let g:clipboard = { 'name': 'tmux', 'cache_enabled': v:true }
  let g:clipboard.copy = { '+': s:tmux_copy, '*': s:tmux_copy }
  let g:clipboard.paste = { '+': s:tmux_paste, '*': s:tmux_paste }
endif

" Supports neovim's python remote plugin integration
let g:python3_host_prog = exepath('nvim-python3')
let g:python2_host_prog = exepath('nvim-python')

" Misc
let further#prefer_modules = v:true
let teleport#update_cwd = v:true
let g:jsx_ext_required = 0
let g:splitjoin_trailing_comma = v:true


" TODO: Make editor/ a real plugin system.
" TODO: Kill the nixplug hack with fiery vengeance.

" Register the dotfiles framework.
func! s:add_plugin_path(plugin) abort
  execute 'set runtimepath+=' . fnameescape(a:plugin)
endfunc

" Resolve path to the editor/ directory.
let s:filename = resolve(expand('<sfile>'))
let s:dotfiles_dir = fnamemodify(s:filename, ':h:h')
let s:editor_dir = s:dotfiles_dir . '/editor'

call s:add_plugin_path(s:editor_dir)

" Check for environment-specific vim settings.
let s:editor_env_preset = dotfiles#env#path('editor')
if isdirectory(s:editor_env_preset)
  call s:add_plugin_path(s:editor_env_preset)
endif

call editor#nixplug#load(dotfiles#path('pkgs/vim-manifest/default.nix'))

" Color scheme
colorscheme onedark

highlight clear ALEWarningSign
highlight ALEWarningSign ctermfg=gray

highlight clear CursorLine
highlight CursorLineNr ctermfg=blue
