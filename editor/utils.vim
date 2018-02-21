" I'd put this function in g:llama.utils, but... keystrokes.
function! s:chomp(string) abort
  return substitute(a:string, '\n$', '', '')
endfunction

" Search every parent directory until a predicate is satisfied.
function! g:llama.utils.SearchDirUpwards(dir, cb) abort
  if a:cb(a:dir)
    return a:dir
  endif

  let l:dir = fnamemodify(a:dir, ':h')

  " Science has gone too far.
  if l:dir is a:dir
    return v:null
  endif

  return g:llama.utils.SearchDirUpwards(l:dir, a:cb)
endfunction

" Locate the directory defining package.json.
function! g:llama.utils.FindProjectRoot() abort
  let l:current_dir = expand('%:p:h')
  let l:Has_pkg_json = {dir -> file_readable(dir . '/package.json')}
  let l:project = g:llama.utils.SearchDirUpwards(l:current_dir, l:Has_pkg_json)

  if l:project is v:null
    let l:project = l:current_dir
  endif

  return l:project
endfunction


" :<range>Author
function! s:find_authors_for_range(start, end) abort
  let l:range = a:start . ',' . a:end
  let l:cmd = 'git blame --porcelain -L ' . l:range . ' -- ' . fnameescape(expand('%:p'))
  let l:my_name = s:chomp(system('git config user.name'))
  let l:blames = systemlist(l:cmd)

  let l:committers = filter(l:blames, {k, v -> v =~# '^committer '})
  let l:authors = map(l:committers, {k, v -> substitute(v, 'committer ', '', '')})
  let l:uniq_authors = {}

  for l:author in l:authors
    let l:actual_author = l:author ==? 'not committed yet' ? l:my_name : l:author
    let l:uniq_authors[l:actual_author] = l:actual_author
  endfor

  return values(l:uniq_authors)
endfunction

function! s:find_line_author() abort range
  let l:authors = s:find_authors_for_range(a:firstline, a:lastline)

  echo join(l:authors, "\n")
endfunction

command! -range Author <line1>,<line2>call <SID>find_line_author()

" :Node repl
function! s:open_node_repl() abort
  let l:project = g:llama.utils.FindProjectRoot()

  copen
  execute 'lcd ' . fnameescape(l:project)
  execute 'term node'
  normal! A
endfunction

command! Node call <SID>open_node_repl()

" :Reset (resets the file to HEAD state)
function! s:git_reset_file() abort
  let l:file = fnameescape(expand('%:p'))
  let l:symlink_pointer = system('readlink ' . l:file)

  " Attempt to resolve symlinks.
  if len(l:symlink_pointer)
    let l:file = s:chomp(l:symlink_pointer)
  endif

  " system(...) uses the cwd context. Not good if
  " you execute this from a different repo.
  let l:original_cwd = getcwd()
  cd %:p:h

  call system('git reset -- ' . l:file)
  call system('git checkout -- ' . l:file)

  execute 'cd! ' . fnameescape(l:original_cwd)

  silent edit!
  silent write
endfunction

command! Reset call s:git_reset_file()
