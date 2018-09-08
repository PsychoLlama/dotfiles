" Locate the directory defining package.json.
func! editor#js#FindPackageRoot(...) abort
  let l:containing_dir = get(a:000, 0, expand('%:p:h'))

  " Make sure it's a directory.
  if !isdirectory(l:containing_dir)
    let l:containing_dir = fnamemodify(l:containing_dir, ':p:h')
  endif

  " Search upwards for a package.json file.
  call execute('lcd ' . l:containing_dir)
  let l:pkg_path = findfile('package.json', ';')
  silent lcd -

  " Resolve an absolute path to the directory.
  if strlen(l:pkg_path)
    return fnamemodify(l:pkg_path, ':p:h')
  endif

  return v:null
endfunc

" Detect if the given file is a JavaScript test file.
func! editor#js#IsTestFile(...) abort
  let l:file_path = get(a:000, 0, expand('%:p'))
  let l:filename = fnamemodify(l:file_path, ':t')

  " Does the filename match something like
  if l:filename =~# '\v\.(test|spec)\.(j|t)sx?$'
    return v:true
  endif

  " Fall back to a directory pattern.
  if l:file_path =~# glob2regpat('**/{__tests__,tests}/**')
    return v:true
  endif

  return v:false
endfunc

func! s:FindTestDirectoriesAbove(file_path) abort
  let l:test_dirname = '__tests__'

  " Find the containing directory.
  let l:containing_dir = a:file_path
  if !isdirectory(l:containing_dir)
    let l:containing_dir = fnamemodify(l:containing_dir, ':h')
  endif

  " Not every test directory is named the same. Provide an escape hatch.
  if exists('*editor#env#ResolveTestDirectoryName')
    let l:test_dirname = editor#env#ResolveTestDirectoryName(a:file_path)
  endif

  " Find every test directory upwards of the given file.
  call execute('lcd ' . fnameescape(l:containing_dir))
  let l:test_dirs = finddir(l:test_dirname, ';', -1)
  call map(l:test_dirs, "fnamemodify(v:val, ':p')")
  silent lcd -

  return l:test_dirs
endfunc

" Given a path to the source file, search every test
" directory above this one for a file name that matches.
func! editor#js#LocateTestFile(...) abort
  " Absolute path to the source file.
  let l:file_path = fnamemodify(get(a:000, 0, expand('%')), ':p')
  let l:filename = fnamemodify(l:file_path, ':t')
  let l:test_dirs = s:FindTestDirectoriesAbove(l:file_path)

  " Search every test directory upwards...
  for l:test_dir in l:test_dirs

    " Look at every test file...
    for l:test_file in glob(l:test_dir . '**/*.js', 0, v:true)
      let l:no_suffix = fnamemodify(l:test_file, ':t')
      let l:no_suffix = substitute(l:no_suffix, '\v\.(test|spec)', '', '')

      " Check to see if the name matches our source file.
      if l:no_suffix is? l:filename
        return l:test_file
      endif
    endfor
  endfor

  return v:null
endfunc
