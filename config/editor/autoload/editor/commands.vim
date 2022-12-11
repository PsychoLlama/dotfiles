" :Node repl
func! editor#commands#node() abort
  if !executable('node')
    echohl Error
    echon 'Error:'
    echohl Clear
    echon ' No node executable.'
    return v:null
  endif

  let l:project = editor#js#find_package_root()

  if l:project is# v:null
    let l:project = editor#util#resolve_directory()
  endif

  new Node Repl
  wincmd J
  resize 10
  execute 'lcd ' . fnameescape(l:project)
  setlocal nonumber signcolumn=no
  call termopen('yarn node')
  normal! A
endfunc


" :Perm +x
func! editor#commands#permissions(...) abort
  let l:file_path = expand('%:p')
  let l:file = fnameescape(l:file_path)

  if !filereadable(l:file_path)
    echo 'Wait, you sure this is a file?'
    return
  endif

  if len(a:000) == 0
    echo getfperm(l:file)
    return
  endif

  let l:output = system('chmod ' . a:1 . ' ' . l:file)

  if v:shell_error
    echoerr l:output
  endif
endfunc
