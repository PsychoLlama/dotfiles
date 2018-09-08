" Resolve a given path or the current file.
func! s:ResolvePath(args) abort
  let l:path = get(a:args, 0, expand('%'))
  return fnamemodify(l:path, ':p')
endfunc

" Locate the directory defining package.json.
func! editor#js#FindPackageRoot(...) abort
  let l:containing_dir = s:ResolvePath(a:000)

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
  let l:file_path = s:ResolvePath(a:000)
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

func! s:RemoveTestSuffix(filename) abort
  let l:no_suffix = fnamemodify(a:filename, ':t')
  let l:no_suffix = substitute(l:no_suffix, '\v\.(test|spec)', '', '')

  return l:no_suffix
endfunc

" Given a path to the source file, search every test
" directory above this one for a file name that matches.
func! editor#js#LocateTestFile(...) abort
  " Absolute path to the source file.
  let l:file_path = s:ResolvePath(a:000)
  let l:filename = fnamemodify(l:file_path, ':t')
  let l:test_dirs = s:FindTestDirectoriesAbove(l:file_path)

  " Search every test directory upwards...
  for l:test_dir in l:test_dirs

    " Look at every test file...
    for l:test_file in glob(l:test_dir . '**/*.js', 0, v:true)
      let l:no_suffix = s:RemoveTestSuffix(l:test_file)

      " Check to see if the name matches our source file.
      if l:no_suffix is? l:filename
        return l:test_file
      endif
    endfor
  endfor

  return v:null
endfunc

func! s:Submatch(string, regex) abort
  let l:results = matchlist(a:string, a:regex)

  " Fail gracefully in case of some really weird syntax.
  if empty(l:results)
    return v:null
  endif

  return l:results[1]
endfunc

func! s:ExtractImportPath(line) abort
  let l:regex = {
        \   'import': '\vimport\s*["|'']',
        \   'from': '\vfrom\s*["|'']',
        \   'require': '\vrequire\s*\(',
        \ }

  " import { ... } from 'path'
  if a:line =~# l:regex.from
    let l:expr = l:regex.from . '(.*)["|'']'
    let l:path = s:Submatch(a:line, l:expr)

    return l:path
  endif

  " import 'path'
  if a:line =~# l:regex.import
    let l:expr = l:regex.import . '(.*)["|'']'
    let l:path = s:Submatch(a:line, l:expr)

    return l:path
  endif

  " require('path')
  if a:line =~# l:regex.require
    let l:str = '["|''|`]'
    let l:expr = l:regex.require . '\s*' . l:str . '(.*)' . l:str
    let l:path = s:Submatch(a:line, l:expr)

    return l:path
  endif

  return v:null
endfunc

func! s:IsRelativeImport(path) abort
  return a:path[0] is# '.'
endfunc

" Scrape test file imports looking for something named similarly.
" Probably a hack, but more reliable and performant than
" depth-scanning tons of files looking for a source file with
" the same name.
func! s:SearchForPlausibleImports(file_path, no_suffix) abort
  let l:imports = getline('^', '$')
  call map(l:imports, 's:ExtractImportPath(v:val)')
  call filter(l:imports, 'v:val isnot# v:null')
  call filter(l:imports, 's:IsRelativeImport(v:val)')
  let l:no_suffix = fnamemodify(a:no_suffix, ':r')

  for l:relative_import in l:imports
    let l:import_filename = fnamemodify(l:relative_import, ':t:r')

    if l:import_filename is? l:no_suffix
      let l:test_dir = fnamemodify(a:file_path, ':h') . '/'
      let l:import_path = simplify(l:test_dir . '/' . l:relative_import)
      let l:src_file = glob(l:import_path . '*')

      " Assume it's either a false positive or the import is bad.
      " Possible failure if multiple results are returned.
      if !filereadable(l:src_file)
        continue
      endif

      " Looks close enough!
      return l:src_file
    endif
  endfor

  return v:null
endfunc

" See if it's in the grandparent directory. If not, scan
" the test file's imports.
func! editor#js#LocateSourceFile(...) abort
  let l:file_path = s:ResolvePath(a:000)
  let l:no_suffix = s:RemoveTestSuffix(l:file_path)
  let l:grandparent_dir = fnamemodify(l:file_path, ':h:h')
  let l:src_file = glob(l:grandparent_dir . '/' . l:no_suffix)

  if filereadable(l:src_file)
    return l:src_file
  endif

  return s:SearchForPlausibleImports(l:file_path, l:no_suffix)
endfunc
