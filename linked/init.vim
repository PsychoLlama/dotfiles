scriptencoding utf-8

" Resolve path to the editor/ directory.
let s:filename = resolve(expand('<sfile>'))
let s:dotfiles_dir = fnamemodify(s:filename, ':h:h')
let s:editor_dir = s:dotfiles_dir . '/editor'

execute 'set runtimepath+=' . fnameescape(s:editor_dir)

" A namespace for shared vimscript functions (mostly consumed from
" ~/dotfiles-env).
let g:llama = { 'utils': {} }

let s:main_settings = s:dotfiles_dir . '/editor/settings.vim'
let s:plugin_config = s:dotfiles_dir . '/editor/plugins.vim'
let s:mappings = s:dotfiles_dir . '/editor/mappings.vim'

" RELEASE THE (plugin) KRAKEN
execute 'source ' . fnameescape(s:main_settings)
execute 'source ' . fnameescape(s:plugin_config)
execute 'source ' . fnameescape(s:mappings)
