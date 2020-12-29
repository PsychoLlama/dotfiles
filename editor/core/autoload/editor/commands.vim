" :SN -- Echo the (S)yntax (N)ame under the cursor.
func! editor#commands#syntax_name() abort
  let l:syn_id = synID(line('.'), col('.'), v:true)
  let l:syn_name = synIDattr(l:syn_id, 'name')

  if empty(l:syn_name)
    echo 'No syntax ID. Lame.'
    return
  endif

  " Prints syntax metadata.
  execute 'highlight ' . l:syn_name
endfunc


" :Node repl
func! editor#commands#node() abort
  call assert#(executable('node'), 'No node executable.')
  let l:project = editor#js#find_package_root()

  if l:project is# v:null
    let l:project = editor#util#resolve_directory()
  endif

  new Node Repl
  wincmd J
  resize 10
  execute 'lcd ' . fnameescape(l:project)
  call termopen('node')
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
