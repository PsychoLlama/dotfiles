func! s:AddPluginPath(plugin) abort
  execute 'set runtimepath+=' . fnameescape(a:plugin)
endfunc

" Resolve path to the editor/ directory.
let s:filename = resolve(expand('<sfile>'))
let s:dotfiles_dir = fnamemodify(s:filename, ':h:h')
let s:editor_dir = s:dotfiles_dir . '/editor'

call s:AddPluginPath(s:editor_dir)

" Check for environment-specific vim settings.
let s:editor_env_preset = expand('~/dotfiles-env/editor')
if isdirectory(s:editor_env_preset)
  call s:AddPluginPath(s:editor_env_preset)
  runtime! autoload/editor/env.vim
endif

" Runtime paths added in the dotfiles preset don't take effect.
" This is a workaround until I find a cleaner approach.
if exists('*editor#env#GetPluginPaths')
  let s:plugins = editor#env#GetPluginPaths()

  for s:plugin in s:plugins
    call s:AddPluginPath(s:plugin)
  endfor
endif

" My vimrc has grown beyond the grasp of a single file.
" Files are managed similarly to a vim plugin, organized into modules.
" Most content can be found under editor/autoload/editor/*
call editor#settings#Init()
call editor#plugins#Init()
call editor#mappings#Init()
