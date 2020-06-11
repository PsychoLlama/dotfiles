let s:MANIFEST_NAME = 'nixplug.json'

func! editor#nixplug#load(nixfile) abort
  let l:plug_path = fnamemodify(a:nixfile, ':h')
  let l:manifest_path = simplify(l:plug_path . '/' . s:MANIFEST_NAME)

  if !filereadable(l:manifest_path)
    echom 'Installing:' a:nixfile
    let l:cmd = 'nix-build ' . fnameescape(l:plug_path)
    let l:cmd .= ' --out-link ' . fnameescape(l:manifest_path)
    call system(l:cmd)
    echom 'All vim plugins were installed!'
  endif

  let l:manifest = json_decode(readfile(l:manifest_path))

  call plug#begin()
  for [l:packpath, l:options] in l:manifest.plugins
    call plug#(l:packpath, l:options)
  endfor
  call plug#end()
endfunc
