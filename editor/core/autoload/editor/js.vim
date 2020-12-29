" Resolve a given path or the current file.
func! s:resolve_path(args) abort
  let l:path = get(a:args, 0, expand('%'))
  return fnamemodify(l:path, ':p')
endfunc

" Locate the directory defining package.json.
func! editor#js#find_package_root(...) abort
  let l:containing_dir = s:resolve_path(a:000)

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

func! s:submatch(string, regex) abort
  let l:results = matchlist(a:string, a:regex)

  " Fail gracefully in case of some really weird syntax.
  if empty(l:results)
    return v:null
  endif

  return l:results[1]
endfunc

func! s:extract_import_path(line) abort
  let l:regex = {
        \   'import': '\vimport\s*["|'']',
        \   'from': '\vfrom\s*["|'']',
        \   'require': '\vrequire\s*\(',
        \ }

  " import { ... } from 'path'
  if a:line =~# l:regex.from
    let l:expr = l:regex.from . '(.*)["|'']'
    let l:path = s:submatch(a:line, l:expr)

    return l:path
  endif

  " import 'path'
  if a:line =~# l:regex.import
    let l:expr = l:regex.import . '(.*)["|'']'
    let l:path = s:submatch(a:line, l:expr)

    return l:path
  endif

  " require('path')
  if a:line =~# l:regex.require
    let l:str = '["|''|`]'
    let l:expr = l:regex.require . '\s*' . l:str . '(.*)' . l:str
    let l:path = s:submatch(a:line, l:expr)

    return l:path
  endif

  return v:null
endfunc

func! s:is_relative_import(path) abort
  return a:path[0] is# '.'
endfunc

func! s:read_packages(package_list) abort
  let l:packages = []

  for l:package in a:package_list
    let l:contents = join(readfile(l:package), "\n")
    let l:json_contents = json_decode(l:contents)

    call add(l:packages, [l:json_contents, l:package])
  endfor

  return l:packages
endfunc

" Infer the test framework from the package's test script.
" Only supports Jest because Jest is Best.
func! s:extract_test_command(test_script) abort
  let l:jest_scripts = ['freighter-scripts']

  for l:jest_script in l:jest_scripts
    if stridx(a:test_script, l:jest_script) > -1
      return 'jest'
    endif
  endfor

  if stridx(a:test_script, 'react-scripts') > -1
    return 'react-scripts test'
  endif

  return a:test_script
endfunc

" Find the test script that controls the given project.
" Searches upwards to support monorepos.
func! s:get_test_runner(...) abort
  let l:file_path = s:resolve_path(a:000)
  let l:package_paths = findfile('package.json', l:file_path . ';', -1)
  let l:packages = s:read_packages(l:package_paths)

  for [l:package, l:package_path] in l:packages
    let l:scripts = get(l:package, 'scripts', {})
    let l:main_test_script = get(l:scripts, 'test', v:null)
    let l:test_script = get(l:scripts, 'test:unit', l:main_test_script)

    if l:test_script is# v:null || type(l:test_script) !=# v:t_string
      continue
    endif

    if l:test_script =~# 'no test specified'
      continue
    endif

    let l:command = s:extract_test_command(l:test_script)
    let l:project_dir = fnamemodify(l:package_path, ':h')
    return { 'command': l:command, 'project': l:project_dir }
  endfor

  " Nothing found.
  return v:null
endfunc

func! editor#js#get_test_command_for_path(...) abort
  let l:path = s:resolve_path(a:000)
  let l:runner = s:get_test_runner(l:path)
  let l:runner = deepcopy(l:runner)

  if l:runner is# v:null
    return v:null
  endif

  " Make the file path project relative.
  let l:test_path = '.' . l:path[strlen(l:runner.project):]

  if l:runner.command =~# '\v(jest|react-scripts)'
    let l:runner.command = 'yarn -s run ' . l:runner.command
    let l:runner.command .= ' --watch --collectCoverage=false '
    let l:runner.command .= shellescape(l:test_path)
  else
    let l:runner.command = 'yarn -s run ' . l:runner.command
    let l:runner.command .= ' --watch ' . shellescape(l:test_path)
  endif

  return l:runner
endfunc

func! editor#js#open_package_root() abort
  let l:root = editor#js#find_package_root()

  if l:root is# v:null
    echo "It doesn't look like you're in a package."
    return
  endif

  execute 'edit ' . fnameescape(l:root)
  lcd %:p
endfunc

func! editor#js#get_package_json(...) abort
  let l:path = s:resolve_path(a:000)
  let l:root = editor#js#find_package_root(l:path)
  let l:pkg_json_path = l:root . '/package.json'

  if !filereadable(l:pkg_json_path)
    echohl Error
    echon 'Error:'
    echohl Clear
    echon " Can't find the package.json file."
    return
  endif

  let l:contents = join(readfile(l:pkg_json_path), ' ')
  return json_decode(l:contents)
endfunc

func! editor#js#is_javascript() abort
  return &filetype =~# '\v\C(javascript|typescript)'
endfunc
