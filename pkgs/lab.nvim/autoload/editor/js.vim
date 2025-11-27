" Resolve a given path or the current file.
func! s:resolve_path(args) abort
  let l:path = get(a:args, 0, expand('%'))
  return fnamemodify(l:path, ':p')
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
    let l:project_dir = fnamemodify(l:package_path, ':p:h')
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


  " Try to detect the test runner so we can put it in watch mode.
  if l:runner.command =~# '\C\Vjest'
    let l:runner.command = 'npm exec --no ' . l:runner.command
    let l:runner.command .= ' --watch --collectCoverage=false ' . shellescape(l:test_path)
  elseif l:runner.command =~# '\C\Vvitest'
    let l:runner.command = 'npm exec --no vitest' " Watch mode is the default.
  else
    let l:runner.command = 'npm exec --no -- ' . l:runner.command
    let l:runner.command .= ' --watch ' . shellescape(l:test_path)
  endif

  return l:runner
endfunc
