" Trim dangling newlines. Mostly from system commands.
func! editor#util#chomp(string) abort
  return substitute(a:string, '\n$', '', '')
endfunc

" Search every parent directory until a predicate is satisfied.
func! editor#util#search_dir_upwards(dir, cb) abort
  if a:cb(a:dir)
    return a:dir
  endif

  let l:dir = fnamemodify(a:dir, ':h')

  " Science has gone too far.
  if l:dir is a:dir
    return v:null
  endif

  return editor#util#search_dir_upwards(l:dir, a:cb)
endfunc

" Turn a file or dir into a candidate for system('cd ...')
" Used to set $PWD before calling a command.
func! editor#util#get_escaped_dir(file) abort
  let l:target = isdirectory(a:file) ? a:file : fnamemodify(a:file, ':h')

  return fnameescape(l:target)
endfunc

" Execute a system command in a directory.
func! editor#util#exec_in_dir(dir, cmd) abort
  let l:cmd = 'cd ' . editor#util#get_escaped_dir(a:dir) . '; '
  let l:cmd .= a:cmd

  return systemlist(l:cmd)
endfunc

func! editor#util#resolve_directory(...) abort
  let l:path = get(a:000, 0, expand('%:p'))

  if isdirectory(l:path)
    return l:path
  endif

  return fnamemodify(l:path, ':h')
endfunc
