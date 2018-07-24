func! s:AddPluginPath(plugin) abort
  execute 'set runtimepath+=' . fnameescape(a:plugin)
endfunc

" Resolve path to the editor/ directory.
let s:filename = resolve(expand('<sfile>'))
let s:dotfiles_dir = fnamemodify(s:filename, ':h:h')
let s:editor_dir = s:dotfiles_dir . '/editor'

call s:AddPluginPath(s:editor_dir)

" Check for environment-specific vim settings.
let s:editor_env_preset = dotfiles#env#Path('editor')
if isdirectory(s:editor_env_preset)
  call s:AddPluginPath(s:editor_env_preset)
endif

" My vimrc has grown beyond the grasp of a single file.
" Files are managed similarly to a vim plugin, organized into modules.
runtime! autoload/editor/{settings,plugins,mappings}.vim
