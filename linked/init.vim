" Resolve path to the editor/ directory.
let s:filename = resolve(expand('<sfile>'))
let s:dotfiles_dir = fnamemodify(s:filename, ':h:h')
let s:editor_dir = s:dotfiles_dir . '/editor'

execute 'set runtimepath+=' . fnameescape(s:editor_dir)

" My vimrc has grown beyond the grasp of a single file.
" Files are managed similarly to a vim plugin, organized into modules.
" Most content can be found under editor/autoload/editor/*
call editor#settings#Init()
call editor#plugins#Init()
call editor#mappings#Init()
