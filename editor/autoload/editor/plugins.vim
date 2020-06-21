scriptencoding utf-8

" Check the dotfiles preset for more vim plugins.
func! s:LoadExternalPlugins() abort
  let l:file_path = dotfiles#env#Path('editor/autoload/editor/env.vim')
  if filereadable(l:file_path)
    execute 'source ' . l:file_path
  endif

  if exists('*editor#env#Plugins')
    call editor#env#Plugins()
  endif
endfunc

" Set python executable paths. Used for remote neovim plugins.
let g:python3_host_prog = exepath('nvim-python3')

if executable('python2')
  let g:python2_host_prog = exepath('python2')
endif

call editor#nixplug#load(dotfiles#Path('pkgs/vim-manifest/default.nix'))

" Color scheme
let g:onedark_termcolors=16
colorscheme onedark
syntax on

highlight clear ALEWarningSign
highlight ALEWarningSign ctermfg=gray

set cursorline
highlight clear CursorLine
highlight CursorLineNr ctermfg=blue

" Linting.
nnoremap <silent><leader>a :ALEDetail<cr>

let g:ale_javascript_prettier_use_local_config = 1
let g:ale_sh_shellcheck_options = '-e SC2155'
let g:ale_sign_warning = '!'
let g:ale_sign_error = 'x'
let g:ale_fix_on_save = 1

let g:ale_linters = {}
let g:ale_linters.javascript = ['eslint', 'flow']
let g:ale_linters.bash = ['shellcheck']
let g:ale_linters.graphql = ['gqlint']
let g:ale_linters.sh = ['shellcheck']
let g:ale_linters.rust = ['rls']
let g:ale_linters.vim = ['vint']
let g:ale_linters.java = []
let g:ale_pattern_options = {
      \   '.*/node_modules/.*': { 'ale_enabled': v:false },
      \ }

" Autoformatters.
let g:ale_fixers = {}
let g:ale_fixers.javascript = ['prettier']
let g:ale_fixers.typescript = g:ale_fixers.javascript
let g:ale_fixers.rust = ['rustfmt']

augroup adjust_ale_preview_pane
  autocmd!
  autocmd FileType ale-preview wincmd J
augroup END

" File browser config.
let g:netrw_list_hide = '^.DS_Store$,^.git/$,^\.\./$,^\./$'
let g:netrw_localrmdir = 'rm -r'
let g:netrw_use_errorwindow = 0
let g:netrw_banner = 0

let further#prefer_modules = v:true
let teleport#update_cwd = v:true

let s:js_testing_conventions = {
      \   'file_naming_conventions': ['{name}.test.{ext}'],
      \   'directory_naming_conventions': ['__tests__'],
      \   'file_extensions': ['js'],
      \ }

let s:ts_testing_conventions = {
      \   'file_naming_conventions': ['{name}.test.{ext}'],
      \   'directory_naming_conventions': ['__tests__'],
      \   'file_extensions': ['ts', 'tsx', 'js'],
      \ }

let s:py_testing_conventions = {
      \   'file_naming_conventions': ['test_{name}.{ext}', '{name}.{ext}'],
      \   'directory_naming_conventions': ['tests'],
      \   'file_extensions': ['py'],
      \ }

let s:vim_testing_conventions = {
      \   'file_naming_conventions': ['{name}.{ext}'],
      \   'directory_naming_conventions': ['tests'],
      \   'file_extensions': ['vim', 'vader'],
      \ }

call alternaut#RegisterLanguage('javascript.jsx', s:js_testing_conventions)
call alternaut#RegisterLanguage('typescript', s:ts_testing_conventions)
call alternaut#RegisterLanguage('typescript.tsx', s:ts_testing_conventions)
call alternaut#RegisterLanguage('python', s:py_testing_conventions)
call alternaut#RegisterLanguage('vim', s:vim_testing_conventions)
call alternaut#RegisterLanguage('vader', s:vim_testing_conventions)

" Use vim-jsx for .js extensions too.
let g:jsx_ext_required = 0

" Always add trailing commas when expanding objects.
let g:splitjoin_trailing_comma = v:true

" Neovim's Buit-in system clipboard integration.
let g:clipboard = !len($TMUX) ? v:null : {
      \   'name': 'tmux',
      \   'copy': {
      \      '+': 'tmux load-buffer -',
      \      '*': 'tmux load-buffer -',
      \    },
      \   'paste': {
      \      '+': 'tmux save-buffer -',
      \      '*': 'tmux save-buffer -',
      \   },
      \   'cache_enabled': 1,
      \ }
