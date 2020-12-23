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

" My vimrc has grown beyond the grasp of a single file.
" Files are managed similarly to a vim plugin, organized into modules.
runtime! autoload/editor/plugins.vim
runtime! autoload/editor/mappings.vim
