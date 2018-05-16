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
  let s:runtimes = editor#env#GetPluginPaths()

  " Runtime paths added in the dotfiles preset don't take effect.
  " This is a workaround until I find a cleaner approach.
  for s:runtime in s:runtimes
    call s:AddPluginPath(s:runtime)
  endfor
endif

" My vimrc has grown beyond the grasp of a single file.
" Files are managed similarly to a vim plugin, organized into modules.
" Most content can be found under editor/autoload/editor/*
call editor#settings#Init()
call editor#plugins#Init()
call editor#mappings#Init()
