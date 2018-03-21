scriptencoding utf-8

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
let s:main_settings = s:dotfiles_dir . '/editor/settings.vim'
let s:plugin_config = s:dotfiles_dir . '/editor/plugins.vim'
let s:utilities = s:dotfiles_dir . '/editor/utils.vim'
let s:mappings = s:dotfiles_dir . '/editor/mappings.vim'

" RELEASE THE (plugin) KRAKEN
execute 'source ' . fnameescape(s:main_settings)
execute 'source ' . fnameescape(s:plugin_config)
execute 'source ' . fnameescape(s:utilities)
execute 'source ' . fnameescape(s:mappings)
